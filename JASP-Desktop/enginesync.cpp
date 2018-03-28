//
// Copyright (C) 2013-2018 University of Amsterdam
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
#include "jsonredirect.h"
#include "processinfo.h"
#include "common.h"
#include "appinfo.h"
#include "qutils.h"
#include "tempfiles.h"

using namespace boost::interprocess;
using namespace std;

EngineSync::EngineSync(Analyses *analyses, DataSetPackage *package, QObject *parent = 0)
	: QObject(parent)
{
	_analyses = analyses;
	_package = package;

	connect(_analyses, SIGNAL(analysisAdded(Analysis*)), this, SLOT(sendMessages()));
	connect(_analyses, SIGNAL(analysisOptionsChanged(Analysis*)), this, SLOT(sendMessages()));
	connect(_analyses, SIGNAL(analysisToRefresh(Analysis*)), this, SLOT(sendMessages()));
	connect(_analyses, SIGNAL(analysisSaveImage(Analysis*)), this, SLOT(sendMessages()));
    connect(_analyses, SIGNAL(analysisEditImage(Analysis*)), this, SLOT(sendMessages()));

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

#ifndef JASP_DEBUG
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
		_engineStates.push_back(engineState::idle);
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
#ifdef JASP_DEBUG
	std::cout << "send " << analysis->id() << " to process " << processNo << "\n";
	std::cout.flush();
#endif

	string perform;

	if (analysis->status() == Analysis::Empty)
	{
		std::cout <<"analysis->status() == Analysis::Empty\n" << std::flush;
		perform = "init";
		analysis->setStatus(Analysis::Initing);
	}
	else if (analysis->status() == Analysis::SaveImg)
	{
		perform = "saveImg";
	}
    else if (analysis->status() == Analysis::EditImg)
    {
        perform = "editImg";
    }
	else if (analysis->status() == Analysis::Aborting)
	{
		std::cout <<"analysis->status() == Analysis::Aborting\n" << std::flush;
		perform = "abort";
		analysis->setStatus(Analysis::Aborted);
	}
	else
	{
		std::cout <<"analysis->status() something else\n" << std::flush;
		perform = "run";
		analysis->setStatus(Analysis::Running);
	}

	_analysesInProgress[processNo]	= analysis;
	_engineStates[processNo]		= engineState::analysis;

	Json::Value json = Json::Value(Json::objectValue);

	json["id"] = analysis->id();
	json["perform"] = perform;
	json["requiresInit"] = analysis->requiresInit();
	json["revision"] = analysis->revision();

	if (analysis->status() != Analysis::Aborted)
	{
		json["name"] = analysis->name();
		json["title"] = analysis->title();
        if (perform == "saveImg" || perform == "editImg") {
			json["image"] = analysis->getSaveImgOptions();
		}
		else
		{
			json["dataKey"] = analysis->dataKey();
			json["stateKey"] = analysis->stateKey();
			json["resultsMeta"] = analysis->resultsMeta();
			json["options"] = analysis->options()->asJSON();
		}
		Json::Value settings;
		settings["ppi"] = _ppi;

		json["settings"] = settings;
	}

	string str = json.toStyledString();
	_channels[processNo]->send(str);

#ifdef JASP_DEBUG
	cout << str << "\n";
	cout.flush();
#endif

}

void EngineSync::process()
{
	for (size_t i = 0; i < _channels.size(); i++)
	{
		if (_engineStates[i] == engineState::idle)
			continue;

		IPCChannel *channel = _channels[i];
		string data;

		if (channel->receive(data))
		{
#ifdef JASP_DEBUG
			std::cout << "message received\n";
			std::cout << data << "\n";
			std::cout.flush();
#endif

			Json::Reader reader;
			Json::Value json;
			reader.parse(data, json);

			switch(_engineStates[i])
			{
			case engineState::filter:
			{
				if(json.get("filterResult", Json::Value(Json::intValue)).isArray()) //If the result is an array then it came from the engine.
				{
					std::vector<bool> filterResult;
					for(Json::Value & jsonResult : json.get("filterResult", Json::Value(Json::arrayValue)))
						filterResult.push_back(jsonResult.asBool());

					processNewFilterResult(filterResult);
				}
				else
					emit filterErrorTextChanged(QString::fromStdString(json.get("filterError", "something went wrong").asString()));
				
				
				_engineStates[i] = engineState::idle;
				
				break;
			}
				
			case engineState::rcode:
			{
				//Do some RCode magic				
				_engineStates[i] = engineState::idle;
				
				if (json.get("rCodeResult", Json::Value(Json::intValue)).isString()) {
					std::cout << "R Code returned: " << json.get("rCodeResult", "") << std::flush;
					emit rCodeReturned(QString::fromStdString(json.get("rCodeResult", "").asString()));
				} 
				
				if (json.get("rCodeError", Json::Value(Json::intValue)).isString()) {
					std::cout << "R Error returned: " << json.get("rCodeError", "") << std::flush;
				}
				
				
				break;
			}
				
			case engineState::analysis:
			{
				Analysis *analysis	= _analysesInProgress[i];
				
				int id				= json.get("id", -1).asInt();
				int revision		= json.get("revision", -1).asInt();
				int progress		= json.get("progress", -1).asInt();
				Json::Value results = json.get("results", Json::nullValue);
				string status		= json.get("status", "error").asString();
	
	
				if (analysis->id() != id || analysis->revision() != revision)
					continue;
	
				std::cout << status << "\n" << std::flush;
	
	
				if (status == "error" || status == "exception")
				{
					analysis->setStatus(status == "error" ? Analysis::Error : Analysis::Exception);
					analysis->setResults(results);
					clearAnalysesInProgress(i);
					
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
					clearAnalysesInProgress(i);
					sendMessages();
				}
				else if (status == "imageEdited")
				{
					analysis->setStatus(Analysis::Complete);
					analysis->setImageEdited(results);
					clearAnalysesInProgress(i);
					sendMessages();
				}
				else if (status == "complete")
				{
					analysis->setStatus(Analysis::Complete);
					analysis->setResults(results);
					clearAnalysesInProgress(i);
					sendMessages();
				}
				else if (status == "inited")
				{
					if (analysis->isAutorun())
						analysis->setStatus(Analysis::Inited);
					else
						analysis->setStatus(Analysis::InitedAndWaiting);
	
					analysis->setResults(results);
					clearAnalysesInProgress(i);
					sendMessages();
				}
				else if (status == "running" && analysis->status() == Analysis::Initing)
				{
					analysis->setStatus(Analysis::Running);
					analysis->setResults(results, progress);
				}
				else if (analysis->status() == Analysis::Running)
				{
					analysis->setResults(results, progress);
				}
				else
				{
					sendMessages();
				}
				
				break;
			}
			}
		}
	}
	
	processScriptQueue();
}

void EngineSync::processNewFilterResult(std::vector<bool> filterResult)
{
	if(_package == NULL || _package->dataSet == NULL)
		return;
	
	_package->dataFilter = dataFilter.toStdString(); //remember the filter that was last used and actually gave results.
	_package->dataSet->setFilterVector(filterResult);

	emit filterUpdated();
	emit filterErrorTextChanged("");
}

void EngineSync::sendFilter(QString generatedFilter, QString filter)
{
	waitingScripts.push(new RFilterStore(generatedFilter, filter));
}

void EngineSync::sendRCode(QString rCode)
{
	waitingScripts.push(new RScriptStore(rCode));
}

void EngineSync::processScriptQueue()
{
	for(int i=0; i<_engineStates.size(); i++)
		if(_engineStates[i] == engineState::idle)
		{
			if(waitingScripts.size() == 0)
				return;

			RScriptStore * waiting = waitingScripts.front();
			waitingScripts.pop();
			
			if(waiting->typeScript == engineState::rcode)
				runScriptOnProcess(waiting, i);
			else if(waiting->typeScript == engineState::filter)
				runScriptOnProcess((RFilterStore*)waiting, i);
			
			delete waiting; //clean up
		}
}

void EngineSync::runScriptOnProcess(RFilterStore * filterStore, int processNo)
{
	Json::Value json = Json::Value(Json::objectValue);

	json["generatedFilter"] = filterStore->generatedfilter.toStdString();

	dataFilter = filterStore->script == "" ? "*" : filterStore->script;
	json["filter"] = dataFilter.toStdString();
	

	string str = json.toStyledString();
	_channels[processNo]->send(str);
	_engineStates[processNo] = engineState::filter;
}

void EngineSync::runScriptOnProcess(RScriptStore * scriptStore, int processNo)
{

	Json::Value json = Json::Value(Json::objectValue);

	json["rCode"] = scriptStore->script.toStdString();
	
	string str = json.toStyledString();
	_channels[processNo]->send(str);
	
	_engineStates[processNo] = engineState::rcode;
}

void EngineSync::sendMessages()
{	
	std::cout << "void EngineSync::sendMessages()\n" << std::flush;
	
	for (size_t i = 0; i < _engineStates.size(); i++) // this loop handles changes in running analyses
		if (_engineStates[i] == engineState::analysis)
		{
			Analysis *analysis = _analysesInProgress[i];

			if (analysis->status() == Analysis::Empty)
			{ 
				sendToProcess(i, analysis);
			}
			else if (analysis->status() == Analysis::Aborting)
			{
				sendToProcess(i, analysis);
				clearAnalysesInProgress(i);
			}
		}

	for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
	{
		Analysis *analysis = *itr;
		if (analysis == NULL)
			continue;
		
		if (analysis->status() == Analysis::Empty || analysis->status() == Analysis::SaveImg || analysis->status() == Analysis::EditImg)
		{
			bool sent = false;

			for (size_t i = 0; i < _engineStates.size(); i++)
			{
				if (_engineStates[i] == engineState::idle) //must be idle (no filter/rcode being run)
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
#ifndef JASP_DEBUG
			for (size_t i = 1; i < _engineStates.size(); i++) // don't perform 'runs' on process 0, only inits.
#else
			for (size_t i = 0; i < _engineStates.size(); i++)
#endif
			{
				if (_engineStates[i] == engineState::idle)
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
	QString rHomePath = programDir.absoluteFilePath("../Frameworks/R.framework/Versions/" + QString::fromStdString(AppInfo::getRVersion()) + "/Resources");
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

	env.insert("LD_LIBRARY_PATH",	rHome.absoluteFilePath("lib") + ":" + rHome.absoluteFilePath("library/RInside/lib") + ":" + rHome.absoluteFilePath("library/Rcpp/lib") + ":" + rHome.absoluteFilePath("site-library/RInside/lib") + ":" + rHome.absoluteFilePath("site-library/Rcpp/lib") + ":/app/lib/:/app/lib64/");
	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("R_LIBS",			programDir.absoluteFilePath("R/library") + ":" + rHome.absoluteFilePath("library") + ":" + rHome.absoluteFilePath("site-library"));

#endif

	QProcess *slave = new QProcess(this);
	slave->setProcessChannelMode(QProcess::ForwardedChannels);
	slave->setProcessEnvironment(env);

#ifdef __WIN32__
	/*
	On Windows, QProcess uses the Win32 API function CreateProcess to
	start child processes.In some casedesirable to fine-tune
	the parameters that are passed to CreateProcess.
	This is done by defining a CreateProcessArgumentModifier function and passing it
	to setCreateProcessArgumentsModifier

	bInheritHandles [in]
	If this parameter is TRUE, each inheritable handle in the calling process
	is inherited by the new process. If the parameter is FALSE, the handles
	are not inherited.
	*/

	slave->setCreateProcessArgumentsModifier([] (QProcess::CreateProcessArguments *args)
	{
#ifndef QT_DEBUG
		args->inheritHandles = false;
#endif
	});
#endif

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
