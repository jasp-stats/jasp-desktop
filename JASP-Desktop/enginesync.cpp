//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "enginesync.h"

#include <QApplication>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QDebug>

#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/foreach.hpp>

#include "lib_json/json.h"

#include "processinfo.h"
#include "common.h"
#include "appinfo.h"
#include "qutils.h"
#include "tempfiles.h"

using namespace boost::interprocess;
using namespace std;

EngineSync::EngineSync(Analyses *analyses, QObject *parent = 0)
	: QObject(parent)
{
	_analyses = analyses;
	_engineStarted = false;

	_log = NULL;
	_ppi = 96;

	connect(_analyses, SIGNAL(analysisAdded(Analysis*)), this, SLOT(sendMessages()));
	connect(_analyses, SIGNAL(analysisOptionsChanged(Analysis*)), this, SLOT(sendMessages()));
	connect(_analyses, SIGNAL(analysisToRefresh(Analysis*)), this, SLOT(sendMessages()));
	connect(_analyses, SIGNAL(analysisSaveImage(Analysis*)), this, SLOT(sendMessages()));

	// delay start so as not to increase program start up time
	QTimer::singleShot(100, this, SLOT(deleteOrphanedTempFiles()));
}

EngineSync::~EngineSync()
{
	if (_engineStarted)
	{
		for (size_t i = 0; i < _slaveProcesses.size(); i++)
		{
			_slaveProcesses[i]->terminate();
			_slaveProcesses[i]->kill();
		}

		tempfiles_deleteAll();
	}

	shared_memory_object::remove(_memoryName.c_str());
}

void EngineSync::start()
{
	if (_engineStarted)
		return;

	_engineStarted = true;

	try {

		unsigned long pid = ProcessInfo::currentPID();

		stringstream ss;
		ss << "JASP-IPC-" << pid;

		_memoryName = ss.str();

		_channels.push_back(new IPCChannel(_memoryName, 0));

#ifdef QT_NO_DEBUG
		_channels.push_back(new IPCChannel(_memoryName, 1));
		_channels.push_back(new IPCChannel(_memoryName, 2));
		_channels.push_back(new IPCChannel(_memoryName, 3));
#endif

	}
	catch (interprocess_exception e)
	{
		qDebug() << "interprocess exception! " << e.what() << "\n";
		throw e;
	}

	for (uint i = 0; i < _channels.size(); i++)
	{
		_analysesInProgress.push_back(NULL);
		startSlaveProcess(i);
	}

	QTimer *timer;

	timer = new QTimer(this);
	connect(timer, SIGNAL(timeout()), this, SLOT(process()));
	timer->start(50);

	timer = new QTimer(this);
	connect(timer, SIGNAL(timeout()), this, SLOT(heartbeatTempFiles()));
	timer->start(30000);
}

bool EngineSync::engineStarted()
{
	return _engineStarted;
}

void EngineSync::setLog(ActivityLog *log)
{
	_log = log;
}

void EngineSync::setPPI(int ppi)
{
	_ppi = ppi;
}

void EngineSync::sendToProcess(int processNo, Analysis *analysis)
{
#ifdef QT_DEBUG
	std::cout << "send " << analysis->id() << " to process " << processNo << "\n";
	std::cout.flush();
#endif

	string perform;

	if (analysis->status() == Analysis::Empty)
	{
		perform = "init";
		analysis->setStatus(Analysis::Initing);
	}
	else if (analysis->status() == Analysis::SaveImg)
	{
		perform = "saveImg";
		analysis->setStatus(Analysis::Initing);
	}
	else if (analysis->status() == Analysis::Aborting)
	{
		perform = "abort";
		analysis->setStatus(Analysis::Aborted);
	}
	else
	{
		perform = "run";
		analysis->setStatus(Analysis::Running);
	}

	_analysesInProgress[processNo] = analysis;

	Json::Value json = Json::Value(Json::objectValue);

	json["id"] = analysis->id();
	json["perform"] = perform;
	json["revision"] = analysis->revision();

	if (analysis->status() != Analysis::Aborted)
	{
		json["name"] = analysis->name();
		if (perform == "saveImg")
			json["image"] = analysis->getSaveImgOptions();
		else
			json["options"] = analysis->options()->asJSON();

		Json::Value settings;
		settings["ppi"] = _ppi;

		json["settings"] = settings;
	}

	string str = json.toStyledString();
	_channels[processNo]->send(str);

#ifndef QT_NO_DEBUG
	cout << str << "\n";
	cout.flush();
#endif

}

void EngineSync::process()
{
	for (size_t i = 0; i < _channels.size(); i++)
	{
		Analysis *analysis = _analysesInProgress[i];

		if (analysis == NULL)
			continue;

		IPCChannel *channel = _channels[i];
		string data;

		if (channel->receive(data))
		{
#ifdef QT_DEBUG
			std::cout << "message received\n";
			std::cout << data << "\n";
			std::cout.flush();
#endif

			Json::Reader reader;
			Json::Value json;
			reader.parse(data, json);

			int id = json.get("id", -1).asInt();
			int revision = json.get("revision", -1).asInt();
			Json::Value results = json.get("results", Json::nullValue);
			string status = json.get("status", "error").asString();

			if (analysis->id() != id || analysis->revision() != revision)
				continue;

			if (status == "error" || status == "exception")
			{
				analysis->setStatus(status == "error" ? Analysis::Error : Analysis::Exception);
				analysis->setResults(results);
				_analysesInProgress[i] = NULL;
				sendMessages();

				if (_log != NULL)
				{
					QString errorMessage = tq(results.get("errorMessage", "").asString());
					QString info = QString("%1,%2").arg(id).arg(errorMessage);
					_log->log("Analysis Error", info);
				}
			}
			else if (status == "imageSaved")
			{
				analysis->setStatus(Analysis::Complete);
				analysis->setImageResults(results);
				_analysesInProgress[i] = NULL;
				sendMessages();
			}
			else if (status == "complete")
			{
				analysis->setStatus(Analysis::Complete);
				analysis->setResults(results);
				_analysesInProgress[i] = NULL;
				sendMessages();
			}
			else if (status == "inited")
			{
				if (analysis->isAutorun())
					analysis->setStatus(Analysis::Inited);
				else
					analysis->setStatus(Analysis::InitedAndWaiting);

				analysis->setResults(results);
				_analysesInProgress[i] = NULL;
				sendMessages();
			}
			else if (status == "running" && analysis->status() == Analysis::Initing)
			{
				analysis->setStatus(Analysis::Running);
				analysis->setResults(results);
			}
			else if (analysis->status() == Analysis::Running)
			{
				analysis->setResults(results);
			}
			else
			{
				sendMessages();
			}
		}

	}
}

void EngineSync::sendMessages()
{
	for (size_t i = 0; i < _analysesInProgress.size(); i++) // this loop handles changes in running analyses
	{
		Analysis *analysis = _analysesInProgress[i];
		if (analysis != NULL)
		{
			if (analysis->status() == Analysis::Empty)
			{
				sendToProcess(i, analysis);
			}
			else if (analysis->status() == Analysis::Aborting)
			{
				sendToProcess(i, analysis);
				_analysesInProgress[i] = NULL;
			}
		}
	}

	for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
	{
		Analysis *analysis = *itr;
		if (analysis == NULL)
			continue;

		if (analysis->status() == Analysis::Empty || analysis->status() == Analysis::SaveImg)
		{
			bool sent = false;

			for (size_t i = 0; i < _analysesInProgress.size(); i++)
			{
				if (_analysesInProgress[i] == NULL)
				{
					sendToProcess(i, analysis);
					sent = true;
					break;
				}
			}

			if (sent == false)  // no free processes left
				return;
		}
		else if (analysis->status() == Analysis::Inited)
		{
#ifndef QT_DEBUG
			for (size_t i = 1; i < _analysesInProgress.size(); i++) // don't perform 'runs' on process 0, only inits.
#else
			for (size_t i = 0; i < _analysesInProgress.size(); i++)
#endif
			{
				if (_analysesInProgress[i] == NULL)
				{
					sendToProcess(i, analysis);
					break;
				}
			}
		}
	}

}

void EngineSync::startSlaveProcess(int no)
{
	QDir programDir = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();
	QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
	QString engineExe = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absoluteFilePath("JASPEngine");

	QStringList args;
	args << QString::number(no);

#ifdef __WIN32__
	QString rHomePath = programDir.absoluteFilePath("R");
#elif __APPLE__
	QString rHomePath = programDir.absoluteFilePath("../Frameworks/R.framework/Versions/" + QString::fromStdString(CURRENT_R_VERSION) + "/Resources");
#else //linux

#ifndef R_HOME
	QString rHomePath = programDir.absoluteFilePath("R/lib/libR.so");
	if (QFileInfo(rHomePath).exists() == false)
		rHomePath = "/usr/lib/R";
#else
	QString rHomePath;
	if (QDir::isRelativePath(R_HOME))
		rHomePath = programDir.absoluteFilePath(R_HOME);
	else
		rHomePath = R_HOME;
#endif

#endif

	QDir rHome(rHomePath);


#ifdef __WIN32__

#if defined(ARCH_32)
#define ARCH_SUBPATH "i386"
#else
#define ARCH_SUBPATH "x64"
#endif

	env.insert("PATH", programDir.absoluteFilePath("R\\library\\RInside\\libs\\" ARCH_SUBPATH) + ";" + programDir.absoluteFilePath("R\\library\\Rcpp\\libs\\" ARCH_SUBPATH) + ";" + programDir.absoluteFilePath("R\\bin\\" ARCH_SUBPATH));
	env.insert("R_HOME", rHome.absolutePath());

	unsigned long processId = ProcessInfo::currentPID();
    args << QString::number(processId);

#undef ARCH_SUBPATH

	env.insert("R_LIBS", rHome.absoluteFilePath("library"));

	env.insert("R_ENVIRON", "something-which-doesnt-exist");
	env.insert("R_PROFILE", "something-which-doesnt-exist");
	env.insert("R_PROFILE_USER", "something-which-doesnt-exist");
	env.insert("R_ENVIRON_USER", "something-which-doesnt-exist");
	env.insert("R_LIBS_SITE", "something-which-doesnt-exist");
	env.insert("R_LIBS_USER", "something-which-doesnt-exist");

#elif __APPLE__

	env.insert("R_HOME", rHome.absolutePath());
	env.insert("R_LIBS", rHome.absoluteFilePath("library") + ":" + programDir.absoluteFilePath("R/library"));

	env.insert("R_ENVIRON", "something-which-doesnt-exist");
	env.insert("R_PROFILE", "something-which-doesnt-exist");
	env.insert("R_PROFILE_USER", "something-which-doesnt-exist");
	env.insert("R_ENVIRON_USER", "something-which-doesnt-exist");
	env.insert("R_LIBS_SITE", "something-which-doesnt-exist");
	env.insert("R_LIBS_USER", "something-which-doesnt-exist");

#else  // linux

	env.insert("LD_LIBRARY_PATH", rHome.absoluteFilePath("lib") + ";" + rHome.absoluteFilePath("library/RInside/lib") + ";" + rHome.absoluteFilePath("library/Rcpp/lib") + ";" + rHome.absoluteFilePath("site-library/RInside/lib") + ";" + rHome.absoluteFilePath("site-library/Rcpp/lib"));
	env.insert("R_HOME", rHome.absolutePath());
	env.insert("R_LIBS", programDir.absoluteFilePath("R/library") + ":" + rHome.absoluteFilePath("library") + ":" + rHome.absoluteFilePath("site-library"));

#endif

	QProcess *slave = new QProcess(this);
	slave->setProcessChannelMode(QProcess::ForwardedChannels);
	slave->setProcessEnvironment(env);
	slave->start(engineExe, args);


	_slaveProcesses.push_back(slave);

	connect(slave, SIGNAL(readyReadStandardOutput()), this, SLOT(subProcessStandardOutput()));
	connect(slave, SIGNAL(readyReadStandardError()), this, SLOT(subProcessStandardError()));
	connect(slave, SIGNAL(error(QProcess::ProcessError)), this, SLOT(subProcessError(QProcess::ProcessError)));
	connect(slave, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(subprocessFinished(int,QProcess::ExitStatus)));
	connect(slave, SIGNAL(started()), this, SLOT(subProcessStarted()));
}

void EngineSync::deleteOrphanedTempFiles()
{
	tempfiles_deleteOrphans();
}

void EngineSync::heartbeatTempFiles()
{
	tempfiles_heartbeat();
}

void EngineSync::subProcessStandardOutput()
{
	QProcess *process = qobject_cast<QProcess *>(this->sender());
	QByteArray data = process->readAllStandardOutput();
	qDebug() << QString(data);
}

void EngineSync::subProcessStandardError()
{
	QProcess *process = qobject_cast<QProcess *>(this->sender());
	qDebug() << process->readAllStandardError();
}

void EngineSync::subProcessStarted()
{
	qDebug() << "subprocess started";
}

void EngineSync::subProcessError(QProcess::ProcessError error)
{
	emit engineTerminated();

	qDebug() << "subprocess error" << error;
}

void EngineSync::subprocessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
	(void)exitStatus;

	emit engineTerminated();

	qDebug() << "subprocess finished" << exitCode;
}

