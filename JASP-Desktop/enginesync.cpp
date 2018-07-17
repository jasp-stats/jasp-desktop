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


EngineSync::EngineSync(Analyses *analyses, DataSetPackage *package, DynamicModules *dynamicModules, QObject *parent = 0)
	: QObject(parent), _analyses(analyses), _package(package), _dynamicModules(dynamicModules)
{
	connect(_analyses,	&Analyses::analysisAdded,							this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::analysisToRefresh,						this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::analysisSaveImage,						this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::analysisEditImage,						this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::analysisOptionsChanged,					this,					&EngineSync::ProcessAnalysisRequests	);

	connect(this,		&EngineSync::moduleLoadingFailed,					_dynamicModules,		&DynamicModules::loadingFailed			);
	connect(this,		&EngineSync::moduleLoadingSucceeded,				_dynamicModules,		&DynamicModules::loadingSucceeded		);
	connect(this,		&EngineSync::moduleInstallationFailed,				_dynamicModules,		&DynamicModules::installationFailed		);
	connect(this,		&EngineSync::moduleInstallationSucceeded,			_dynamicModules,		&DynamicModules::installationSucceeded	);

	// delay start so as not to increase program start up time
	QTimer::singleShot(100, this, &EngineSync::deleteOrphanedTempFiles);
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

			connect(_engines[i],	&EngineRepresentation::rCodeReturned,					this,			&EngineSync::rCodeReturned);

			connect(_engines[i],	&EngineRepresentation::filterUpdated,					this,			&EngineSync::filterUpdated);
			connect(_engines[i],	&EngineRepresentation::dataFilterChanged,				this,			&EngineSync::dataFilterChanged);
			connect(_engines[i],	&EngineRepresentation::processNewFilterResult,			this,			&EngineSync::processNewFilterResult);
			connect(_engines[i],	&EngineRepresentation::filterErrorTextChanged,			this,			&EngineSync::filterErrorTextChanged);

			connect(_engines[i],	&EngineRepresentation::computeColumnSucceeded,			this,			&EngineSync::computeColumnSucceeded);
			connect(_engines[i],	&EngineRepresentation::computeColumnFailed,				this,			&EngineSync::computeColumnFailed);

			connect(_engines[i],	&EngineRepresentation::engineTerminated,				this,			&EngineSync::engineTerminated);

			connect(_engines[i],	&EngineRepresentation::moduleLoadingFailed,				this,			&EngineSync::moduleLoadingFailed);
			connect(_engines[i],	&EngineRepresentation::moduleLoadingSucceeded,			this,			&EngineSync::moduleLoadingSucceeded);
			connect(_engines[i],	&EngineRepresentation::moduleInstallationFailed,		this,			&EngineSync::moduleInstallationFailed);
			connect(_engines[i],	&EngineRepresentation::moduleInstallationSucceeded,		this,			&EngineSync::moduleInstallationSucceeded);

			connect(this,			&EngineSync::ppiChanged,								_engines[i],	&EngineRepresentation::ppiChanged);
		}
	}
	catch (interprocess_exception e)
	{
		qDebug() << "interprocess exception! " << e.what() << "\n";
		throw e;
	}

	QTimer *timerProcess = new QTimer(this), *timerBeat = new QTimer(this);

	connect(timerProcess,	&QTimer::timeout, this, &EngineSync::process);
	connect(timerBeat,		&QTimer::timeout, this, &EngineSync::heartbeatTempFiles);

	timerProcess->start(50);
	timerBeat->start(30000);
}


void EngineSync::process()
{
	for (auto engine : _engines)
		engine->process();
	
	processScriptQueue();
	processDynamicModules();
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


void EngineSync::processDynamicModules()
{
	for(auto engine : _engines)
		if(engine->isIdle())
		{
			if		(_dynamicModules->aModuleNeedsPackagesInstalled())		engine->runModuleRequestOnProcess(_dynamicModules->requestJsonForPackageInstallationRequest());
			else if	(_dynamicModules->aModuleNeedsToBeLoadedInR())			engine->runModuleRequestOnProcess(_dynamicModules->requestJsonForPackageLoadingRequest());

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
			1; // don't perform 'runs' on process 0, "only" inits & filters & rCode & columnComputes & moduleRequests.
#else
			0;
#endif

	for(auto engine : _engines)
		engine->handleRunningAnalysisStatusChanges();

	for (Analysis *analysis : *_analyses)
	{
		if(!idleEngineAvailable())
			return;

		if (analysis == NULL || analysis->isWaitingForModule())
			continue;

		bool canUseFirstEngine	= analysis->isEmpty()	|| analysis->isSaveImg() || analysis->isEditImg();
		bool needsToRun			= canUseFirstEngine		|| analysis->isInited();

		if(needsToRun)
			for (size_t i = canUseFirstEngine ? 0 : initedAnalysesStartIndex; i<_engines.size(); i++)
				if (_engines[i]->isIdle())
				{
					_engines[i]->runAnalysisOnProcess(analysis);
					break;
				}
	}
}

QProcess * EngineSync::startSlaveProcess(int no)
{
	QDir programDir			= QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();
	QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
	QString engineExe		= QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absoluteFilePath("JASPEngine");

	QStringList args;
	args << QString::number(no) << QString::number(ProcessInfo::currentPID());

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
	QString rHomePath = QDir::isRelativePath(R_HOME) ? programDir.absoluteFilePath(R_HOME) : R_HOME;
#endif
#endif

	QDir rHome(rHomePath);


#ifdef __WIN32__

#if defined(ARCH_32)
#define ARCH_SUBPATH "i386"
#else
#define ARCH_SUBPATH "x64"
#endif

	env.insert("PATH",				programDir.absoluteFilePath("R\\library\\RInside\\libs\\" ARCH_SUBPATH) + ";" + programDir.absoluteFilePath("R\\library\\Rcpp\\libs\\" ARCH_SUBPATH) + ";" + programDir.absoluteFilePath("R\\bin\\" ARCH_SUBPATH));
	env.insert("R_HOME",			rHome.absolutePath());

#undef ARCH_SUBPATH

	env.insert("R_LIBS",			rHome.absoluteFilePath("library"));

	env.insert("R_ENVIRON",			"something-which-doesnt-exist");
	env.insert("R_PROFILE",			"something-which-doesnt-exist");
	env.insert("R_PROFILE_USER",	"something-which-doesnt-exist");
	env.insert("R_ENVIRON_USER",	"something-which-doesnt-exist");
	env.insert("R_LIBS_SITE",		"something-which-doesnt-exist");
	env.insert("R_LIBS_USER",		"something-which-doesnt-exist");

#elif __APPLE__

	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("R_LIBS",			rHome.absoluteFilePath("library") + ":" + programDir.absoluteFilePath("R/library"));

	env.insert("R_ENVIRON",			"something-which-doesnt-exist");
	env.insert("R_PROFILE",			"something-which-doesnt-exist");
	env.insert("R_PROFILE_USER",	"something-which-doesnt-exist");
	env.insert("R_ENVIRON_USER",	"something-which-doesnt-exist");
	env.insert("R_LIBS_SITE",		"something-which-doesnt-exist");
	env.insert("R_LIBS_USER",		"/Users/jorisgoosen/.JASP/library");

	std::cout << "env.insert(\"R_LIBS_USER\", \"/Users/jorisgoosen/.JASP/library\"); IS STILL BEING SET! REMOVE THIS BEFORE MERGING INTO DEVELOPMENT!" << std::endl;

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

	connect(slave, &QProcess::readyReadStandardOutput,	this,	&EngineSync::subProcessStandardOutput);
	connect(slave, &QProcess::readyReadStandardError,	this,	&EngineSync::subProcessStandardError);
	connect(slave, &QProcess::finished,					this,	&EngineSync::subprocessFinished);
	connect(slave, &QProcess::started,					this,	&EngineSync::subProcessStarted);
	connect(slave, &QProcess::error,					this,	&EngineSync::subProcessError);

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
	QProcess *process	= qobject_cast<QProcess *>(this->sender());
	QByteArray data		= process->readAllStandardOutput();
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
