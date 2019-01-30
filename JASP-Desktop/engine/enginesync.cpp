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

#include "jsonredirect.h"
#include "processinfo.h"
#include "common.h"
#include "appinfo.h"
#include "utilities/qutils.h"
#include "tempfiles.h"
#include "timers.h"
#include "utilities/appdirs.h"

using namespace boost::interprocess;


EngineSync::EngineSync(Analyses *analyses, DataSetPackage *package, DynamicModules *dynamicModules, QObject *parent = 0)
	: QObject(parent), _analyses(analyses), _package(package), _dynamicModules(dynamicModules)
{
	connect(_analyses,	&Analyses::analysisAdded,							this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::analysisToRefresh,						this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::analysisSaveImage,						this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::analysisEditImage,						this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::analysisRewriteImages,					this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::analysisOptionsChanged,					this,					&EngineSync::ProcessAnalysisRequests	);
	connect(_analyses,	&Analyses::sendRScript,								this,					&EngineSync::sendRCode					);
	connect(this,		&EngineSync::moduleLoadingFailed,					_dynamicModules,		&DynamicModules::loadingFailed			);
	connect(this,		&EngineSync::moduleLoadingSucceeded,				_dynamicModules,		&DynamicModules::loadingSucceeded		);
	connect(this,		&EngineSync::moduleInstallationFailed,				_dynamicModules,		&DynamicModules::installationPackagesFailed		);
	connect(this,		&EngineSync::moduleInstallationSucceeded,			_dynamicModules,		&DynamicModules::installationPackagesSucceeded	);

	// delay start so as not to increase program start up time
	QTimer::singleShot(100, this, &EngineSync::deleteOrphanedTempFiles);
}

EngineSync::~EngineSync()
{
	if (_engineStarted)
	{		
		_engines.clear();
		TempFiles::deleteAll();
	}

	shared_memory_object::remove(_memoryName.c_str());
}

void EngineSync::start(int ppi)
{
	if (_engineStarted)
		return;


	JASPTIMER_START(EngineSync::start());

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

			connect(_engines[i],	&EngineRepresentation::rCodeReturned,					_analyses,		&Analyses::rCodeReturned												);
			connect(_engines[i],	&EngineRepresentation::engineTerminated,				this,			&EngineSync::engineTerminated											);
			connect(_engines[i],	&EngineRepresentation::processNewFilterResult,			this,			&EngineSync::processNewFilterResult										);
			connect(_engines[i],	&EngineRepresentation::processFilterErrorMsg,			this,			&EngineSync::processFilterErrorMsg										);
			connect(_engines[i],	&EngineRepresentation::computeColumnSucceeded,			this,			&EngineSync::computeColumnSucceeded										);
			connect(_engines[i],	&EngineRepresentation::computeColumnFailed,				this,			&EngineSync::computeColumnFailed										);
			connect(_engines[i],	&EngineRepresentation::moduleLoadingFailed,				this,			&EngineSync::moduleLoadingFailedHandler									);
			connect(_engines[i],	&EngineRepresentation::moduleLoadingSucceeded,			this,			&EngineSync::moduleLoadingSucceededHandler								);
			connect(_engines[i],	&EngineRepresentation::moduleInstallationFailed,		this,			&EngineSync::moduleInstallationFailed									);
			connect(_engines[i],	&EngineRepresentation::moduleInstallationSucceeded,		this,			&EngineSync::moduleInstallationSucceeded								);
			connect(_engines[i],	&EngineRepresentation::moduleUnloadingFinished,			this,			&EngineSync::moduleUnloadingFinishedHandler								);
			connect(this,			&EngineSync::ppiChanged,								this,			&EngineSync::refreshAllPlots,					Qt::QueuedConnection	);
			connect(this,			&EngineSync::ppiChanged,								_engines[i],	&EngineRepresentation::ppiChanged										);
			connect(this,			&EngineSync::imageBackgroundChanged,					_engines[i],	&EngineRepresentation::imageBackgroundChanged							);
			connect(_analyses,		&Analyses::analysisRemoved,								_engines[i],	&EngineRepresentation::analysisRemoved									);

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

	emit ppiChanged(ppi);
}

void EngineSync::process()
{
	for (auto engine : _engines)
		engine->process();
	
	processScriptQueue();
	processDynamicModules();
	ProcessAnalysisRequests();
}

void EngineSync::sendFilter(QString generatedFilter, QString filter, int requestID)
{
	if(_waitingFilter == nullptr || _waitingFilter->requestId < requestID)
	{
#ifdef JASP_DEBUG
		std::cout << "waiting filter  with requestid: " << requestID << " is now:\n" << generatedFilter.toStdString() << "\n" << filter.toStdString() << std::endl;
#endif

		_waitingFilter = new RFilterStore(generatedFilter, filter, requestID); //There is no point in having more then one waiting filter is there?
	}
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
	for(auto * engine : _engines)
		if(engine->isIdle())
		{
			if(_waitingScripts.size() == 0 && _waitingFilter == nullptr)
				return;

			if(_waitingFilter != nullptr)
			{
				engine->runScriptOnProcess(_waitingFilter);
				_waitingFilter = nullptr;
			}
			else
			{

				RScriptStore * waiting = _waitingScripts.front();

				switch(waiting->typeScript)
				{
				case engineState::rCode:			engine->runScriptOnProcess(waiting);						break;
				case engineState::filter:			engine->runScriptOnProcess((RFilterStore*)waiting);			break;
				case engineState::computeColumn:	engine->runScriptOnProcess((RComputeColumnStore*)waiting);	break;
				default:							throw std::runtime_error("engineState " + engineStateToString(waiting->typeScript) + " unknown in EngineSync::processScriptQueue()!");
				}

				_waitingScripts.pop();
				delete waiting; //clean up
			}
		}
}


void EngineSync::processDynamicModules()
{
	if(!amICastingAModuleRequestWide() && (_dynamicModules->aModuleNeedsToBeLoadedInR() || _dynamicModules->aModuleNeedsToBeUnloadedInR()))
	{
		resetModuleWideCastVars();

		if(_dynamicModules->aModuleNeedsToBeLoadedInR())		_requestWideCastModuleJson	= _dynamicModules->requestJsonForPackageLoadingRequest();
		else if(_dynamicModules->aModuleNeedsToBeUnloadedInR())	_requestWideCastModuleJson	= _dynamicModules->requestJsonForPackageUnloadingRequest();

		_requestWideCastModuleName = _requestWideCastModuleJson["moduleName"].asString();
	}

	for(auto engine : _engines)
		if(engine->isIdle())
		{
			if		(_dynamicModules->aModuleNeedsPackagesInstalled())															engine->runModuleRequestOnProcess(_dynamicModules->requestJsonForPackageInstallationRequest());
			else if	(!_requestWideCastModuleJson.isNull() && _requestWideCastModuleResults.count(engine->channelNumber()) == 0)	engine->runModuleRequestOnProcess(_requestWideCastModuleJson);
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

	_analyses->applyToSome([&](Analysis * analysis)
	{
		if(!idleEngineAvailable())
			return false;

		if (analysis == nullptr || analysis->isWaitingForModule())
			return true;

		bool canUseFirstEngine	= analysis->isEmpty()	|| analysis->isSaveImg() || analysis->isEditImg() || analysis->isRewriteImgs();
		bool needsToRun			= canUseFirstEngine		|| analysis->isInited();

		if(needsToRun)
			for (size_t i = canUseFirstEngine ? 0 : initedAnalysesStartIndex; i<_engines.size(); i++)
				if (_engines[i]->isIdle())
				{
					_engines[i]->runAnalysisOnProcess(analysis);
					break;
				}

		return true;
	});


}

QProcess * EngineSync::startSlaveProcess(int no)
{
	QDir programDir			= QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();
	QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
	QString engineExe		= QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absoluteFilePath("JASPEngine");

	QStringList args;
	args << QString::number(no) << QString::number(ProcessInfo::currentPID());

	env.insert("TMPDIR", tq(TempFiles::createTmpFolder()));

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

#elif __APPLE__

	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("R_LIBS",			rHome.absoluteFilePath("library") + ":" + programDir.absoluteFilePath("R/library"));

	//env.insert("R_ENVIRON",			"something-which-doesnt-exist");
	//env.insert("R_PROFILE",			"something-which-doesnt-exist");
	//env.insert("R_PROFILE_USER",	"something-which-doesnt-exist");
	//env.insert("R_ENVIRON_USER",	"something-which-doesnt-exist");

#else  // linux
	env.insert("LD_LIBRARY_PATH",	rHome.absoluteFilePath("lib") + ":" + rHome.absoluteFilePath("library/RInside/lib") + ":" + rHome.absoluteFilePath("library/Rcpp/lib") + ":" + rHome.absoluteFilePath("site-library/RInside/lib") + ":" + rHome.absoluteFilePath("site-library/Rcpp/lib") + ":/app/lib/:/app/lib64/");
	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("R_LIBS",			programDir.absoluteFilePath("R/library") + ":" + rHome.absoluteFilePath("library") + ":" + rHome.absoluteFilePath("site-library"));

#endif

	env.insert("R_LIBS_SITE",		"");
	env.insert("R_LIBS_USER",		AppDirs::userRLibrary().toStdString().c_str());

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

	connect(slave, &QProcess::readyReadStandardOutput,								this,	&EngineSync::subProcessStandardOutput);
	connect(slave, &QProcess::readyReadStandardError,								this,	&EngineSync::subProcessStandardError);
	connect(slave, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),	this,	&EngineSync::subprocessFinished);
	connect(slave, &QProcess::started,												this,	&EngineSync::subProcessStarted);
	connect(slave, &QProcess::errorOccurred,										this,	&EngineSync::subProcessError);

	return slave;
}

void EngineSync::deleteOrphanedTempFiles()
{
	TempFiles::deleteOrphans();
}

void EngineSync::heartbeatTempFiles()
{
	TempFiles::heartbeat();
}

void EngineSync::subProcessStandardOutput()
{
	QProcess *process	= qobject_cast<QProcess *>(this->sender());
	QByteArray data		= process->readAllStandardOutput();
	qDebug() << "cout jaspEngine: " << QString(data);
}

void EngineSync::subProcessStandardError()
{
	QProcess *process = qobject_cast<QProcess *>(this->sender());
	qDebug() << "cerr jaspEngine: " << process->readAllStandardError();
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

void EngineSync::subprocessFinished(int exitCode, QProcess::ExitStatus)
{
	emit engineTerminated();

	qDebug() << "subprocess finished" << exitCode;
}

void EngineSync::pause()
{
	//make sure we process any received messages first.
	for(auto engine : _engines)
		engine->process();

	for(EngineRepresentation * e : _engines)
		e->pauseEngine();

	while(!allEnginesPaused())
		for (auto engine : _engines)
			engine->process();
}

void EngineSync::resume()
{
	for(auto * engine : _engines)
		engine->resumeEngine();

	while(!allEnginesResumed())
		for (auto * engine : _engines)
			engine->process();
}

bool EngineSync::allEnginesPaused()
{
	for(auto * engine : _engines)
		if(!engine->paused())
			return false;
	return true;
}

bool EngineSync::allEnginesResumed()
{
	for(auto * engine : _engines)
		if(!engine->resumed())
			return false;
	return true;
}

void EngineSync::moduleLoadingFailedHandler(std::string moduleName, std::string errorMessage, int channelID)
{
#ifdef JASP_DEBUG
	std::cout << "Received EngineSync::moduleLoadingFailedHandler(" << moduleName << ", " << errorMessage << ", " << channelID << ")" << std::endl;
#endif

	if(_requestWideCastModuleName != moduleName)
		throw std::runtime_error("Unexpected module received in EngineSync::moduleLoadingFailed, expected: " + _requestWideCastModuleName + ", but got: " + moduleName);

	_requestWideCastModuleResults[channelID] = errorMessage.size() == 0 ? "error" : errorMessage;

	checkModuleWideCastDone();
}

void EngineSync::moduleLoadingSucceededHandler(std::string moduleName, int channelID)
{
#ifdef JASP_DEBUG
	std::cout << "Received EngineSync::moduleLoadingSucceededHandler(" << moduleName << ", " << channelID << ")" << std::endl;
#endif

	if(_requestWideCastModuleName != moduleName)
		throw std::runtime_error("Unexpected module received in EngineSync::moduleLoadingSucceeded, expected: " + _requestWideCastModuleName + ", but got: " + moduleName);

	_requestWideCastModuleResults[channelID] = "succes";

	checkModuleWideCastDone();
}

void EngineSync::moduleUnloadingFinishedHandler(std::string moduleName, int channelID)
{
#ifdef JASP_DEBUG
	std::cout << "Received EngineSync::moduleUnloadingFinishedHandler(" << moduleName << ", " << channelID << ")" << std::endl;
#endif

	if(_requestWideCastModuleName != moduleName)
		throw std::runtime_error("Unexpected module received in EngineSync::moduleUnloadingFinishedHandler, expected: " + _requestWideCastModuleName + ", but got: " + moduleName);

	_requestWideCastModuleResults[channelID] = "I am an inconsequential message";

	checkModuleWideCastDone();
}


void EngineSync::checkModuleWideCastDone()
{
	if(!_requestWideCastModuleJson.isNull() && _requestWideCastModuleResults.size() == _engines.size())
	{
		if(moduleStatusFromString(_requestWideCastModuleJson["moduleRequest"].asString()) == moduleStatus::loadingNeeded)
		{
			std::string compoundedError = "";
			int failed = 0;

			for(auto * engine : _engines)
			{
				auto res = _requestWideCastModuleResults[engine->channelNumber()];
				if(res != "succes")
				{
					failed ++;
					compoundedError += std::string(compoundedError != "" ? "\n" : "") + "engine " + std::to_string(engine->channelNumber()) + " reported error: " + res;
				}
			}


			if(failed == 0)	emit moduleLoadingSucceeded(_requestWideCastModuleName);
			else			emit moduleLoadingFailed(_requestWideCastModuleName, compoundedError);
		}

		resetModuleWideCastVars();
	}
}

void EngineSync::resetModuleWideCastVars()
{
	_requestWideCastModuleName			= "";
	_requestWideCastModuleJson			= Json::nullValue;
	_requestWideCastModuleResults.clear();
}

void EngineSync::refreshAllPlots(int)
{
	std::set<Analysis*> inProgress;
	for(EngineRepresentation * engine : _engines)
		if(engine->analysisInProgress() != nullptr)
			inProgress.insert(engine->analysisInProgress());

	emit refreshAllPlotsExcept(inProgress);
}
