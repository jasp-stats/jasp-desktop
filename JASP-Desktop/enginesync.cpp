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


EngineSync::EngineSync(Analyses *analyses, DataSetPackage *package, QObject *parent = 0)
	: QObject(parent)
{
	_analyses = analyses;
	_package = package;

	connect(_analyses, SIGNAL(analysisAdded(Analysis*)), this, SLOT(ProcessAnalysisRequests()));
	connect(_analyses, SIGNAL(analysisOptionsChanged(Analysis*)), this, SLOT(ProcessAnalysisRequests()));
	connect(_analyses, SIGNAL(analysisToRefresh(Analysis*)), this, SLOT(ProcessAnalysisRequests()));
	connect(_analyses, SIGNAL(analysisSaveImage(Analysis*)), this, SLOT(ProcessAnalysisRequests()));
	connect(_analyses, SIGNAL(analysisEditImage(Analysis*)), this, SLOT(ProcessAnalysisRequests()));

	// delay start so as not to increase program start up time
	QTimer::singleShot(100, this, SLOT(deleteOrphanedTempFiles()));
}

EngineSync::~EngineSync()
{
	if (_engineStarted)
	{
		_engines.clear();
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
		_memoryName = "JASP-IPC-" + std::to_string(ProcessInfo::currentPID());

#ifdef JASP_DEBUG
		_engines.resize(1);
#else
		_engines.resize(4);
#endif
		for(size_t i=0; i<_engines.size(); i++)
		{
			_engines[i] = new EngineRepresentation(new IPCChannel(_memoryName, i), startSlaveProcess(i), this);

			connect(_engines[i],	&EngineRepresentation::engineTerminated,				this,			&EngineSync::engineTerminated);
			connect(_engines[i],	&EngineRepresentation::filterUpdated,					this,			&EngineSync::filterUpdated);
			connect(_engines[i],	&EngineRepresentation::filterErrorTextChanged,			this,			&EngineSync::filterErrorTextChanged);
			connect(_engines[i],	&EngineRepresentation::rCodeReturned,					this,			&EngineSync::rCodeReturned);
			connect(_engines[i],	&EngineRepresentation::processNewFilterResult,			this,			&EngineSync::processNewFilterResult);
			connect(_engines[i],	&EngineRepresentation::dataFilterChanged,				this,			&EngineSync::dataFilterChanged);
			connect(_engines[i],	&EngineRepresentation::computeColumnSucceeded,			this,			&EngineSync::computeColumnSucceeded);
			connect(_engines[i],	&EngineRepresentation::computeColumnFailed,				this,			&EngineSync::computeColumnFailed);
			connect(this,			&EngineSync::ppiChanged,								_engines[i],	&EngineRepresentation::ppiChanged);
		}
	}
	catch (interprocess_exception e)
	{
		qDebug() << "interprocess exception! " << e.what() << "\n";
		throw e;
	}

	QTimer *timer = new QTimer(this);
	connect(timer, SIGNAL(timeout()), this, SLOT(process()));
	timer->start(50);

	timer = new QTimer(this);
	connect(timer, SIGNAL(timeout()), this, SLOT(heartbeatTempFiles()));
	timer->start(30000);
}


void EngineSync::process()
{
	for (auto engine : _engines)
		engine->process();
	
	processScriptQueue();
	ProcessAnalysisRequests();
}

void EngineSync::processNewFilterResult(std::vector<bool> filterResult)
{
	if(_package == NULL || _package->dataSet() == NULL)
		return;

	
	_package->setDataFilter(_dataFilter.toStdString()); //remember the filter that was last used and actually gave results.
	_package->dataSet()->setFilterVector(filterResult);

	emit filterUpdated();
	emit filterErrorTextChanged("");
}

void EngineSync::sendFilter(QString generatedFilter, QString filter)
{
	_waitingScripts.push(new RFilterStore(generatedFilter, filter));
}

void EngineSync::sendRCode(QString rCode, int requestId)
{
	_waitingScripts.push(new RScriptStore(requestId, rCode));
}

void EngineSync::computeColumn(QString columnName, QString computeCode, Column::ColumnType columnType)
{
	//first we remove the previously sent requests!
	std::queue<RScriptStore*> copiedWaiting(_waitingScripts);
	_waitingScripts = std::queue<RScriptStore*>() ;

	while(copiedWaiting.size() > 0)
	{
		RScriptStore * cur = copiedWaiting.front();
		if(cur->typeScript != engineState::computeColumn || static_cast<RComputeColumnStore*>(cur)->columnName != columnName)
			_waitingScripts.push(cur);
		copiedWaiting.pop();
	}

	_waitingScripts.push(new RComputeColumnStore(columnName, computeCode, columnType));
}

void EngineSync::processScriptQueue()
{
	for(auto engine : _engines)
		if(engine->isIdle())
		{
			if(_waitingScripts.size() == 0)
				return;

			RScriptStore * waiting = _waitingScripts.front();
			_waitingScripts.pop();
			
			switch(waiting->typeScript)
			{
			case engineState::rCode:			engine->runScriptOnProcess(waiting);						break;
			case engineState::filter:			engine->runScriptOnProcess((RFilterStore*)waiting);			break;
			case engineState::computeColumn:	engine->runScriptOnProcess((RComputeColumnStore*)waiting);	break;
			default:							throw std::runtime_error("engineState " + engineStateToString(waiting->typeScript) + " unknown in EngineSync::processScriptQueue()!");
			}
			
			delete waiting; //clean up
		}
}

bool EngineSync::idleEngineAvailable()
{
	for(auto engine : _engines)
		if(engine->isIdle())
			return true;
	return false;
}


void EngineSync::ProcessAnalysisRequests()
{	
	const size_t initedAnalysesStartIndex =
#ifndef JASP_DEBUG
			1; // don't perform 'runs' on process 0, "only" inits & filters & rCode & columnCoputes.
#else
			0;
#endif

	for(auto engine : _engines)
		engine->handleRunningAnalysisStatusChanges();

	for (Analysis *analysis : *_analyses)
	{
		if(!idleEngineAvailable())
			return;

		if (analysis == NULL)
			continue;
		
		if (analysis->isEmpty() || analysis->isSaveImg() || analysis->isEditImg())
		{
			for(auto engine : _engines)
				if(engine->isIdle())
				{
					engine->runAnalysisOnProcess(analysis);
					break;
				}
		}
		else if (analysis->isInited())
			for (size_t i = initedAnalysesStartIndex; i<_engines.size(); i++)
				if (_engines[i]->isIdle())
				{
					_engines[i]->runAnalysisOnProcess(analysis);
					break;
				}
	}
}

QProcess * EngineSync::startSlaveProcess(int no)
{
	QDir programDir = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();
	QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
	QString engineExe = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absoluteFilePath("JASPEngine");

	QStringList args;
	args << QString::number(no);
	args << QString::number(ProcessInfo::currentPID());

#ifdef __WIN32__
	QString rHomePath = programDir.absoluteFilePath("R");
#elif __APPLE__
    QString rHomePath = programDir.absoluteFilePath("../Frameworks/R.framework/Versions/" + QString::fromStdString(AppInfo::getRVersion()) + "/Resources");
#else //linux

#ifndef R_HOME
	QString rHomePath = programDir.absoluteFilePath("R/lib/libR.so");
	if (QFileInfo(rHomePath).exists() == false)
#ifdef FLATPAK_USED
		rHomePath = "/app/lib64/R/";
#else
		rHomePath = "/usr/lib/R/";
#endif
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
	slave->setWorkingDirectory(QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absolutePath());

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

	connect(slave, SIGNAL(readyReadStandardOutput()), this, SLOT(subProcessStandardOutput()));
	connect(slave, SIGNAL(readyReadStandardError()), this, SLOT(subProcessStandardError()));
	connect(slave, SIGNAL(error(QProcess::ProcessError)), this, SLOT(subProcessError(QProcess::ProcessError)));
	connect(slave, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(subprocessFinished(int,QProcess::ExitStatus)));
	connect(slave, SIGNAL(started()), this, SLOT(subProcessStarted()));

	return slave;
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
