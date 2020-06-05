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


#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>

#include "jsonredirect.h"
#include "processinfo.h"
#include "common.h"
#include "appinfo.h"
#include "utilities/qutils.h"
#include "utils.h"
#include "tempfiles.h"
#include "timers.h"
#include "gui/preferencesmodel.h"
#include "utilities/appdirs.h"
#include "log.h"


using namespace boost::interprocess;

EngineSync * EngineSync::_singleton = nullptr;

EngineSync::EngineSync(QObject *parent)
	: QObject(parent)
{
	assert(!_singleton);
	_singleton = this;

	connect(Analyses::analyses(),		&Analyses::sendRScript,								this,						&EngineSync::sendRCode							);
	connect(this,						&EngineSync::moduleLoadingFailed,					DynamicModules::dynMods(),	&DynamicModules::loadingFailed					);
	connect(this,						&EngineSync::moduleLoadingSucceeded,				DynamicModules::dynMods(),	&DynamicModules::loadingSucceeded				);
	connect(this,						&EngineSync::moduleInstallationFailed,				DynamicModules::dynMods(),	&DynamicModules::installationPackagesFailed		);
	connect(this,						&EngineSync::moduleInstallationSucceeded,			DynamicModules::dynMods(),	&DynamicModules::installationPackagesSucceeded	);
	connect(DynamicModules::dynMods(),	&DynamicModules::stopEngines,						this,						&EngineSync::stopEngines						);
	connect(DynamicModules::dynMods(),	&DynamicModules::restartEngines,					this,						&EngineSync::restartEngines						);

	connect(PreferencesModel::prefs(),	&PreferencesModel::plotPPIChanged,					this,						&EngineSync::settingsChanged					);
	connect(PreferencesModel::prefs(),	&PreferencesModel::plotBackgroundChanged,			this,						&EngineSync::settingsChanged					);
	connect(PreferencesModel::prefs(),	&PreferencesModel::languageCodeChanged,				this,						&EngineSync::settingsChanged					);
	connect(PreferencesModel::prefs(),	&PreferencesModel::developerModeChanged,			this,						&EngineSync::settingsChanged					);

    // delay start so as not to increase program start up time 10sec is better than 100ms, because they are orphaned anyway
    QTimer::singleShot(10000, this, &EngineSync::deleteOrphanedTempFiles);

	DataSetPackage::pkg()->setEngineSync(this);

	_memoryName = "JASP-IPC-" + std::to_string(ProcessInfo::currentPID());
}

EngineSync::~EngineSync()
{
	if (_engineStarted)
	{		
		try			{ stopEngines(); }
		catch(...)	{ /* Whatever! */ }

		for(EngineRepresentation * engine : _engines)
			if(!engine->stopped())
				engine->killEngine();

		_engines.clear();
		TempFiles::deleteAll();
	}
	_singleton = nullptr;
}

EngineRepresentation * EngineSync::createNewEngine()
{
	try
	{
		size_t i = _engines.size();

		_engines.push_back(new EngineRepresentation(new IPCChannel(_memoryName, i), startSlaveProcess(i), this));

		connect(_engines[i],			&EngineRepresentation::rCodeReturned,					Analyses::analyses(),	&Analyses::rCodeReturned												);
		connect(_engines[i],			&EngineRepresentation::engineTerminated,				this,					&EngineSync::engineTerminated											);
		connect(_engines[i],			&EngineRepresentation::processNewFilterResult,			this,					&EngineSync::processNewFilterResult										);
		connect(_engines[i],			&EngineRepresentation::filterDone,						this,					&EngineSync::filterDone													);
		connect(_engines[i],			&EngineRepresentation::processFilterErrorMsg,			this,					&EngineSync::processFilterErrorMsg										);
		connect(_engines[i],			&EngineRepresentation::columnDataTypeChanged,			this,					&EngineSync::columnDataTypeChanged,				Qt::QueuedConnection	);
		connect(_engines[i],			&EngineRepresentation::computeColumnSucceeded,			this,					&EngineSync::computeColumnSucceeded,			Qt::QueuedConnection	);
		connect(_engines[i],			&EngineRepresentation::computeColumnFailed,				this,					&EngineSync::computeColumnFailed,				Qt::QueuedConnection	);
		connect(_engines[i],			&EngineRepresentation::moduleLoadingFailed,				this,					&EngineSync::moduleLoadingFailedHandler									);
		connect(_engines[i],			&EngineRepresentation::moduleLoadingSucceeded,			this,					&EngineSync::moduleLoadingSucceededHandler								);
		connect(_engines[i],			&EngineRepresentation::moduleInstallationFailed,		this,					&EngineSync::moduleInstallationFailed									);
		connect(_engines[i],			&EngineRepresentation::moduleInstallationSucceeded,		this,					&EngineSync::moduleInstallationSucceeded								);
		connect(_engines[i],			&EngineRepresentation::moduleUnloadingFinished,			this,					&EngineSync::moduleUnloadingFinishedHandler								);
		connect(_engines[i],			&EngineRepresentation::moduleUninstallingFinished,		this,					&EngineSync::moduleUninstallingFinished									);
		connect(_engines[i],			&EngineRepresentation::logCfgReplyReceived,				this,					&EngineSync::logCfgReplyReceived										);
		connect(_engines[i],			&EngineRepresentation::plotEditorRefresh,				this,					&EngineSync::plotEditorRefresh											);
		connect(_engines[i],			&EngineRepresentation::requestEngineRestart,			this,					&EngineSync::restartEngineAfterCrash									);
		connect(this,					&EngineSync::settingsChanged,							_engines[i],			&EngineRepresentation::settingsChanged									);
		connect(Analyses::analyses(),	&Analyses::analysisRemoved,								_engines[i],			&EngineRepresentation::analysisRemoved									);

		return _engines[i];

	}
	catch (interprocess_exception & e)
	{
		Log::log()  << "interprocess exception! " << e.what() <<  std::endl;
		throw e;
	}
}

void EngineSync::start(int )
{
	JASPTIMER_SCOPE(EngineSync::start);

	if (_engineStarted)
		return;

	_engineStarted = true;

	size_t enginesWanted = 4;
#ifdef JASP_DEBUG
		enginesWanted = 1;
#endif

	for(size_t i=0; i<enginesWanted; i++)
		createNewEngine();


	QTimer	*timerProcess	= new QTimer(this),
			*timerBeat		= new QTimer(this);

	connect(timerProcess,	&QTimer::timeout, this, &EngineSync::process,				Qt::QueuedConnection);
	connect(timerBeat,		&QTimer::timeout, this, &EngineSync::heartbeatTempFiles,	Qt::QueuedConnection);

	timerProcess->start(50);
	timerBeat->start(30000);
}

void EngineSync::restartEngines()
{
	if(_engineStarted)
		return;

	Log::log() << "restarting engines!" << std::endl;

	for(size_t i=0; i<_engines.size(); i++)
	{
		_engines[i]->restartEngine(startSlaveProcess(i));
		Log::log() << "restarted engine " << i << " but should still reload any active (dynamic) modules!"<< std::endl;
	}

	setModuleWideCastVars(DynamicModules::dynMods()->getJsonForReloadingActiveModules());
	logCfgRequest();

	_engineStarted = true;
}

void EngineSync::restartEngineAfterCrash(int nr)
{
	EngineRepresentation * eng = _engines[size_t(nr)];

	eng->restartEngine(startSlaveProcess(nr));
}

void EngineSync::restartKilledEngines()
{
	//Maybe we killed an engine because we wanted to pause or some option changed but the analysis wasn't listening. https://github.com/jasp-stats/INTERNAL-jasp/issues/875
	bool restartedAnEngine = false;

	for(size_t i=0; i<_engines.size(); i++)
		if(_engines[i]->killed())
		{
			_engines[i]->restartEngine(startSlaveProcess(i));
			restartedAnEngine = true;
		}

	if(restartedAnEngine)
		setModuleWideCastVars(DynamicModules::dynMods()->getJsonForReloadingActiveModules());
}

void EngineSync::process()
{
	restartKilledEngines();

	for (auto engine : _engines)
		engine->process();

	processSettingsChanged();
	processFilterScript();

	if(_filterRunning) return; //Do not do anything else while waiting for a filter to return

	processLogCfgRequests();
	processScriptQueue();
	processDynamicModules();
	processAnalysisRequests();

	for (auto engine : _engines)
		if(engine->idle())
			engine->restartAbortedAnalysis();
}

int EngineSync::sendFilter(const QString & generatedFilter, const QString & filter)
{
	_waitingFilter = new RFilterStore(generatedFilter, filter, ++_filterCurrentRequestID);
	Log::log() << "waiting filter with requestid: " << _filterCurrentRequestID << " is now:\n" << generatedFilter.toStdString() << "\n" << filter.toStdString() << std::endl;

	return _filterCurrentRequestID;
}


void EngineSync::sendRCode(const QString & rCode, int requestId, bool whiteListedVersion)
{
	_waitingScripts.push(new RScriptStore(requestId, rCode, engineState::rCode, whiteListedVersion));
}

void EngineSync::computeColumn(const QString & columnName, const QString & computeCode, columnType colType)
{
	//first we remove the previously sent requests for this same column!
	std::queue<RScriptStore*> copiedWaiting(_waitingScripts);
	_waitingScripts = std::queue<RScriptStore*>() ;

	while(copiedWaiting.size() > 0)
	{
		RScriptStore * cur = copiedWaiting.front();
		if(cur->typeScript != engineState::computeColumn || static_cast<RComputeColumnStore*>(cur)->_columnName != columnName)
			_waitingScripts.push(cur);
		copiedWaiting.pop();
	}

	_waitingScripts.push(new RComputeColumnStore(columnName, computeCode, colType));
}

void EngineSync::processFilterScript()
{

	if (!_waitingFilter)
		return;

	Log::log() << "Pausing and resuming engines to make sure nothing else is running when we start the filter." << std::endl;

	//First we make sure nothing else is running before we ask the engine to run the filter
	if(!_filterRunning)
	{
		pause(); //Make sure engines pause/stop
		_filterRunning = true;
		resume();
	}
	else //So previous loop we made sure nothing else is running, and maybe we had to kill an engine to make it understand, followed by a restart. Now we are ready to run the filter
	{
		try
		{
			for (auto *engine : _engines)
				if (engine->idle())
				{
					engine->runScriptOnProcess(_waitingFilter);
					_waitingFilter = nullptr;
					return;
				}

		} catch (...){	Log::log() << "Exception sent in processFilterScript" << std::endl;	}
	}
}

void EngineSync::filterDone(int requestID)
{
	if(requestID != _filterCurrentRequestID)
		return;

	_filterRunning = false; //Allow other stuff to happen
}

void EngineSync::processSettingsChanged()
{
	for(auto * engine : _engines)
		if(engine->shouldSendSettings())
			engine->sendSettings();
}

void EngineSync::processScriptQueue()
{
	try
	{
		for(auto * engine : _engines)
		{
			if(engine->idle() && _waitingScripts.size() > 0)
			{
				RScriptStore * waiting = _waitingScripts.front();

				switch(waiting->typeScript)
				{
				case engineState::rCode:			engine->runScriptOnProcess(waiting);						break;
				//case engineState::filter:			engine->runScriptOnProcess((RFilterStore*)waiting);			break;
				case engineState::computeColumn:	engine->runScriptOnProcess((RComputeColumnStore*)waiting);	break;
				default:							throw std::runtime_error("engineState " + engineStateToString(waiting->typeScript) + " unknown in EngineSync::processScriptQueue()!");
				}

				_waitingScripts.pop();
				delete waiting; //clean up
			}
		}
	}
	catch(...)
	{
		Log::log() << "Exception thrown in processScriptQueue" << std::endl;
	}
}


void EngineSync::processDynamicModules()
{
	if(!amICastingAModuleRequestWide() && (DynamicModules::dynMods()->aModuleNeedsToBeLoadedInR() || DynamicModules::dynMods()->aModuleNeedsToBeUnloadedFromR()))
	{
		if(DynamicModules::dynMods()->aModuleNeedsToBeLoadedInR())			setModuleWideCastVars(DynamicModules::dynMods()->getJsonForPackageLoadingRequest());
		else if(DynamicModules::dynMods()->aModuleNeedsToBeUnloadedFromR())	setModuleWideCastVars(DynamicModules::dynMods()->getJsonForPackageUnloadingRequest());
	}

	if	(!DynamicModules::dynMods()->aModuleNeedsPackagesInstalled() && _requestWideCastModuleJson.isNull())
		return;


	try
	{
		for(auto engine : _engines)
		{
			if(engine->idle())
			{
				if		(DynamicModules::dynMods()->aModuleNeedsPackagesInstalled())															engine->runModuleRequestOnProcess(DynamicModules::dynMods()->getJsonForPackageInstallationRequest());
				else if	(!_requestWideCastModuleJson.isNull() && _requestWideCastModuleResults.count(engine->channelNumber()) == 0)	engine->runModuleRequestOnProcess(_requestWideCastModuleJson);
			}
		}
	}
	catch(...)
	{
		Log::log() << "Exception thrown in processDynamicModules" << std::endl;
	}

}

void EngineSync::processAnalysisRequests()
{	
	for(auto engine : _engines)
		engine->handleRunningAnalysisStatusChanges();

	Analyses::analyses()->applyToSome([&](Analysis * analysis)
	{
		if(analysis && analysis->shouldRun())
		{
			try
			{
				for(auto engine : _engines)
					if(engine->willProcessAnalysis(analysis))
					{
						engine->runAnalysisOnProcess(analysis);
						return true;
					}
			}
			catch(...)	{ Log::log() << "Exception thrown in ProcessAnalysisRequests" << std::endl;	}
		}

		return true;
	});
}

QProcess * EngineSync::startSlaveProcess(int no)
{
	JASPTIMER_SCOPE(EngineSync::startSlaveProcess);
	QDir programDir			= QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();
	QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
	QString engineExe		= QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absoluteFilePath("JASPEngine");

	QStringList args;
	args << QString::number(no) << QString::number(ProcessInfo::currentPID()) << QString::fromStdString(Log::logFileNameBase) << QString::fromStdString(Log::whereStr());

	env.insert("TMPDIR", tq(TempFiles::createTmpFolder()));

#ifdef _WIN32
	QString rHomePath = programDir.absoluteFilePath("R");
#elif defined(__APPLE__)
    QString rHomePath = programDir.absoluteFilePath("../Frameworks/R.framework/Versions/" + QString::fromStdString(AppInfo::getRVersion()) + "/Resources");
#else //linux

#ifndef R_HOME
	QString rHomePath = programDir.absoluteFilePath("R/lib/libR.so");
	if (QFileInfo(rHomePath).exists() == false)
#ifdef FLATPAK_USED
		rHomePath = "/app/lib64/R/"; //Tools/flatpak/org.jaspstats.JASP.json also sets R_HOME to /app/lib64 for 32bits...
#else
		rHomePath = "/usr/lib/R/";
#endif
#else
	QString rHomePath = QDir::isRelativePath(R_HOME) ? programDir.absoluteFilePath(R_HOME) : R_HOME;
#endif
#endif

	QDir rHome(rHomePath);
	Log::log() << "R_HOME set to " << rHomePath.toStdString() << std::endl;

	QString custom_R_library = "";
#ifdef JASP_DEBUG
	// allow an environment variables to specify the location of packages
	if (env.contains("JASP_R_Library"))
		custom_R_library = ":" + env.value("JASP_R_Library");
#endif
#ifdef _WIN32
	//Windows has *special needs*, so let's make sure it can understand R_HOME later on. Not sure if it is necessary but it couldn't hurt, right?
	QString rHomeWin = "";

	for(auto & kar : rHome.absolutePath())
		rHomeWin += kar != '/' ? QString(kar) : "\\";

#if defined(ARCH_32)
#define ARCH_SUBPATH "i386"
#else
#define ARCH_SUBPATH "x64"
#endif

	env.insert("PATH",				programDir.absoluteFilePath("R\\library\\RInside\\libs\\" ARCH_SUBPATH) + ";" + programDir.absoluteFilePath("R\\library\\Rcpp\\libs\\" ARCH_SUBPATH) + ";" + programDir.absoluteFilePath("R\\bin\\" ARCH_SUBPATH));
	env.insert("R_HOME",			rHomeWin);
	env.insert("JAGS_HOME",			programDir.absoluteFilePath("JAGS/"));

#undef ARCH_SUBPATH

	env.insert("R_LIBS",			rHomeWin + "\\library");

	env.insert("R_ENVIRON",			"something-which-doesn't-exist");
	env.insert("R_PROFILE",			"something-which-doesn't-exist");
	env.insert("R_PROFILE_USER",	"something-which-doesn't-exist");
	env.insert("R_ENVIRON_USER",	"something-which-doesn't-exist");
	env.insert("LC_CTYPE",			"C"); //To force utf-8 output from gettext et al. This is most likely only necessary on Windows but it can't hurt right?

#elif __APPLE__

	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("JASP_R_HOME",		rHome.absolutePath()); //Used by the modified R script in jasp-required-files/Framework/etc/bin to make sure we use the actual R of JASP! (https://github.com/jasp-stats/INTERNAL-jasp/issues/452)
	env.insert("R_LIBS",			rHome.absoluteFilePath("library") + ":" + programDir.absoluteFilePath("R/library"));
	env.insert("JAGS_HOME",			programDir.absoluteFilePath("JAGS/"));

	//env.insert("R_ENVIRON",			"something-which-doesnt-exist");
	//env.insert("R_PROFILE",			"something-which-doesnt-exist");
	//env.insert("R_PROFILE_USER",	"something-which-doesnt-exist");
	//env.insert("R_ENVIRON_USER",	"something-which-doesnt-exist");

	env.insert("LC_CTYPE",			"UTF-8"); //This isn't really a locale but seems necessary to get proper output from gettext on mac

#else  // linux
	env.insert("LD_LIBRARY_PATH",	rHome.absoluteFilePath("lib") + ":" + rHome.absoluteFilePath("library/RInside/lib") + ":" + rHome.absoluteFilePath("library/Rcpp/lib") + ":" + rHome.absoluteFilePath("site-library/RInside/lib") + ":" + rHome.absoluteFilePath("site-library/Rcpp/lib") + ":/app/lib/:/app/lib64/");
	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("R_LIBS",			programDir.absoluteFilePath("R/library") + custom_R_library + ":" + rHome.absoluteFilePath("library") + ":" + rHome.absoluteFilePath("site-library"));

	//Let's just trust linux and *not set* LC_CTYPE at all. It'll be fine.
#endif


	env.insert("R_LIBS_SITE",		"");
	env.insert("R_LIBS_USER",		AppDirs::userRLibrary().toStdString().c_str());

	QProcess *slave = new QProcess(this);
	slave->setProcessChannelMode(QProcess::ForwardedChannels);
	slave->setProcessEnvironment(env);
	slave->setWorkingDirectory(QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absolutePath());

#ifdef _WIN32
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

void EngineSync::stopEngines()
{
	if(!_engineStarted) return;

	auto timeout = QDateTime::currentSecsSinceEpoch() + 10;

	//make sure we process any received messages first.
	for(auto engine : _engines)
		engine->process();

	_engineStarted = false;

	for(EngineRepresentation * e : _engines)
		e->stopEngine();

	while(!allEnginesStopped())
		if(timeout < QDateTime::currentSecsSinceEpoch())
		{
			std::cerr << "Waiting for engine to reply stopRequest took longer than timeout, killing it/them.." << std::endl;
			for(EngineRepresentation * e : _engines)
				if(!e->stopped() && !e->killed())
					e->killEngine();

			break;
		}
		else
		for (auto * engine : _engines)
			engine->process();

	timeout = QDateTime::currentSecsSinceEpoch() + 10;

	bool stillRunning;

	do
	{
		QApplication::processEvents(); //Otherwise we will not get feedback from QProcess (aka finished)

		stillRunning = false;

		for (auto * engine : _engines)
			if(engine->jaspEngineStillRunning())
				stillRunning = true;
	}
	while(stillRunning && timeout > QDateTime::currentSecsSinceEpoch()); //Let's give good old jaspEngines some time to shutdown gracefully

	if(stillRunning)
		Log::log() << "Waiting for engine to stop took longer than timeout.." << std::endl;
	else
		Log::log() << "Engines stopped" << std::endl;
}

void EngineSync::pause()
{
	JASPTIMER_SCOPE(EngineSync::pause);

	if(!_engineStarted) return;

	//make sure we process any received messages first.
	for(auto engine : _engines)
		engine->process();

	for(EngineRepresentation * e : _engines)
		e->pauseEngine();

	long tryTill = Utils::currentSeconds() + KILLTIME; //Ill give the engine 1 sec to respond

	while(!allEnginesPaused() && tryTill >= Utils::currentSeconds())
		for (auto * engine : _engines)
			engine->process();

	for (auto * engine : _engines)
		if(!engine->paused())
			engine->killEngine(true);
}

void EngineSync::resume()
{
	JASPTIMER_SCOPE(EngineSync::resume);

	if(!_engineStarted)
		return;

	bool restartedAnEngine = false;

	for(size_t i=0; i<_engines.size(); i++)
		if(!_engines[i]->jaspEngineStillRunning())
		{
			_engines[i]->restartEngine(startSlaveProcess(i));
			restartedAnEngine = true;
		}
		else
			_engines[i]->resumeEngine();

	while(!allEnginesResumed())
		for (auto * engine : _engines)
			engine->process();

	if(restartedAnEngine)
		setModuleWideCastVars(DynamicModules::dynMods()->getJsonForReloadingActiveModules());
}

bool EngineSync::allEnginesStopped()
{
	for(auto * engine : _engines)
		if(!engine->stopped())
			return false;
	return true;
}

bool EngineSync::allEnginesPaused()
{
	for(auto * engine : _engines)
		if(!engine->paused() && !engine->initializing()) //Initializing is also sort of paused I guess
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

bool EngineSync::allEnginesInitializing()
{
	for(auto * engine : _engines)
		if(!engine->initializing())
			return false;
	return true;
}

void EngineSync::moduleLoadingFailedHandler(const QString & moduleName, const QString & errorMessage, int channelID)
{
	Log::log() << "Received EngineSync::moduleLoadingFailedHandler(" << moduleName.toStdString() << ", " << errorMessage.toStdString() << ", " << channelID << ")" << std::endl;

	if(_requestWideCastModuleName != moduleName.toStdString())
		throw std::runtime_error("Unexpected module received in EngineSync::moduleLoadingFailed, expected: " + _requestWideCastModuleName + ", but got: " + moduleName.toStdString());

	_requestWideCastModuleResults[channelID] = errorMessage.size() == 0 ? "error" : errorMessage.toStdString();

	checkModuleWideCastDone();
}

void EngineSync::moduleLoadingSucceededHandler(const QString & moduleName, int channelID)
{
	Log::log() << "Received EngineSync::moduleLoadingSucceededHandler(" << moduleName.toStdString() << ", " << channelID << ")" << std::endl;

	if(_requestWideCastModuleName != moduleName.toStdString())
		throw std::runtime_error("Unexpected module received in EngineSync::moduleLoadingSucceeded, expected: " + _requestWideCastModuleName + ", but got: " + moduleName.toStdString());

	_requestWideCastModuleResults[channelID] = "succes";

	checkModuleWideCastDone();
}

void EngineSync::moduleUnloadingFinishedHandler(const QString & moduleName, int channelID)
{
	Log::log() << "Received EngineSync::moduleUnloadingFinishedHandler(" << moduleName.toStdString() << ", " << channelID << ")" << std::endl;

	if(_requestWideCastModuleName != moduleName.toStdString())
		throw std::runtime_error("Unexpected module received in EngineSync::moduleUnloadingFinishedHandler, expected: " + _requestWideCastModuleName + ", but got: " + moduleName.toStdString());

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


			if(failed == 0)	emit moduleLoadingSucceeded(QString::fromStdString(_requestWideCastModuleName));
			else			emit moduleLoadingFailed(QString::fromStdString(_requestWideCastModuleName), QString::fromStdString(compoundedError));
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

void EngineSync::setModuleWideCastVars(Json::Value newVars)
{
	resetModuleWideCastVars();

	_requestWideCastModuleName			= newVars["moduleName"].asString();
	_requestWideCastModuleJson			= newVars;
}

void EngineSync::refreshAllPlots()
{
	std::set<Analysis*> inProgress;
	for(EngineRepresentation * engine : _engines)
		if(engine->analysisInProgress() != nullptr)
			inProgress.insert(engine->analysisInProgress());

	//If an analysis is empty it means it will be reran anyway, so rewriteImgs is pointless
	Analyses::analyses()->applyToAll([&](Analysis * analysis)
	{
		if(analysis->isEmpty())
			inProgress.insert(analysis);
	});

	emit refreshAllPlotsExcept(inProgress);
}


void EngineSync::logCfgRequest()
{
	for(EngineRepresentation * e : _engines)
		_logCfgRequested.insert(e->channelNumber());
}

void EngineSync::logCfgReplyReceived(size_t channelNr)
{
	_logCfgRequested.erase(channelNr);
}

void EngineSync::processLogCfgRequests()
{
	if (_logCfgRequested.size() == 0)
		return;


	try
	{
		for(size_t channelNr : _logCfgRequested)
			if(_engines[channelNr]->idle())
				_engines[channelNr]->sendLogCfg();
	}
	catch (...)
	{
		Log::log() << "Exception thrown in processLogCfgRequests" << std:: endl << std::flush;
	}
}

void EngineSync::cleanUpAfterClose()
{
	//try { stopEngines(); } //Tends to go wrong when the engine was already killed (for instance because it didnt want to pause)
	try {	pause(); }
	catch(unexpectedEngineReply e) {} // If we are cleaning up after close we can get all sorts of things, lets just ignore them.

	while(_waitingScripts.size() > 0)
	{
		delete _waitingScripts.front();
		_waitingScripts.pop();
	}

	if(_waitingFilter)
		delete _waitingFilter;
	_waitingFilter = nullptr;

	TempFiles::clearSessionDir();

	for(EngineRepresentation * e : _engines)
		e->cleanUpAfterClose();

	try { resume(); }
	//try { restartEngines(); }
	catch(unexpectedEngineReply e) {}


}

std::string	EngineSync::currentState() const
{
	try
	{
		std::stringstream out;

		for(size_t i=0; i<_engines.size(); i++)
			try			{ out << _engines[i]->currentState() << "\n"; }
			catch(...)	{ out << "Something is wrong with engine " << i << "...\n"; }

		return out.str();
	}
	catch(...)
	{
		return "EngineSync::currentState() did not work...\n";
	}

}

EngineRepresentation *	EngineSync::createRCmdEngine()
{
	EngineRepresentation * rCmdEngine = createNewEngine();

	rCmdEngine->setRunsAnalysis(false);
	rCmdEngine->setRunsUtility(	false);
	rCmdEngine->setRunsRCmd(	true);

	return rCmdEngine;
}

void EngineSync::destroyEngine(EngineRepresentation * engine)
{
	if(!engine) return;

	for(int i = _engines.size() - 1; i>=0; i--)
		if(_engines[i] == engine)
			_engines.erase(_engines.begin() + i);

	delete engine;
}
