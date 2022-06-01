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

#include <json/json.h>
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
#include "utilities/qutils.h"
#include "utilities/processhelper.h"

using namespace boost::interprocess;

EngineSync * EngineSync::_singleton = nullptr;

EngineSync::EngineSync(QObject *parent)
	: QAbstractListModel(parent)
{
	assert(!_singleton);
	_singleton = this;
	
	using namespace Modules;

	connect(Analyses::analyses(),		&Analyses::sendRScript,								this,						&EngineSync::sendRCode							);
	connect(this,						&EngineSync::moduleInstallationFailed,				this,						&EngineSync::moduleInstallationFailedHandler	);
	connect(this,						&EngineSync::moduleInstallationFailed,				DynamicModules::dynMods(),	&DynamicModules::installationPackagesFailed,	Qt::DirectConnection);
	connect(this,						&EngineSync::moduleInstallationSucceeded,			DynamicModules::dynMods(),	&DynamicModules::installationPackagesSucceeded,	Qt::DirectConnection);


	connect(PreferencesModel::prefs(),	&PreferencesModel::plotPPIChanged,					this,						&EngineSync::settingsChanged					);
	connect(PreferencesModel::prefs(),	&PreferencesModel::plotBackgroundChanged,			this,						&EngineSync::settingsChanged					);
	connect(PreferencesModel::prefs(),	&PreferencesModel::languageCodeChanged,				this,						&EngineSync::settingsChanged					);
	connect(PreferencesModel::prefs(),	&PreferencesModel::developerModeChanged,			this,						&EngineSync::settingsChanged					);
	connect(PreferencesModel::prefs(),	&PreferencesModel::githubPatCustomChanged,			this,						&EngineSync::settingsChanged					);
	connect(PreferencesModel::prefs(),	&PreferencesModel::githubPatUseDefaultChanged,		this,						&EngineSync::settingsChanged					);

	// delay start so as not to increase program start up time 10sec is better than 100ms, because they are orphaned anyway
	// Except, that it might somehow cause a crash? If the timer goes off while waiting for a download from OSF than it might remove the files while making them..
	// So lets put it on 500ms...
	QTimer::singleShot(500, this, &EngineSync::deleteOrphanedTempFiles);

	DataSetPackage::pkg()->setEngineSync(this);

	_memoryName = "JASP-IPC-" + std::to_string(ProcessInfo::currentPID());
}

EngineSync::~EngineSync()
{
	try			{ stopEngines(); }
	catch(...)	{ /* Whatever! */ }

	for(EngineRepresentation * engine : _engines)
		if(!engine->stopped())
			engine->killEngine();

	_moduleEngines.clear();
	_engines.clear();


	delete _rCmderChannel;
	_rCmderChannel	= nullptr;
	_rCmder			= nullptr;

	TempFiles::deleteAll();

	_singleton = nullptr;
}


int EngineSync::rowCount(const QModelIndex &) const
{
	return _engines.size();
}

std::vector<EngineRepresentation*>  EngineSync::orderedEngines() const
{
	std::vector<EngineRepresentation*> ordered(_engines.begin(), _engines.end());

	std::sort(ordered.begin(), ordered.end(), [](EngineRepresentation * l, EngineRepresentation * r) { return l->channelNumber() < r->channelNumber(); });

	return ordered;
}

QVariant EngineSync::data(const QModelIndex &index, int role) const
{
	if(!enginesListRolesValid(role))
		role=Qt::DisplayRole;

	if(index.row() >= rowCount() || index.row() < 0)
		return QVariant();

	auto ordered = orderedEngines();

	EngineRepresentation * engine = ordered[index.row()];

	if(role < Qt::UserRole)
	{
		if(role == Qt::DisplayRole)	return "Engine " + tq(std::to_string(engine->channelNumber()));
		return QVariant();
	}

	switch(static_cast<enginesListRoles>(role))
	{
	case enginesListRoles::channel:			return int(engine->channelNumber());
	case enginesListRoles::module:			return tq(engine->installingModule() ?  engine->moduleRequested() : engine->module());
	case enginesListRoles::engineState:		return engineStateToQString(engine->state());
	case enginesListRoles::running:			return !engine->killed() && !engine->stopped();
	case enginesListRoles::idle:			return engine->idle();
	case enginesListRoles::idleSoon:		return engine->idleSoon();
	case enginesListRoles::analysisStatus:	return engine->analysisStatus();
	case enginesListRoles::runsWhat:		return QString("Runs ") +(engine->runsAnalysis() ? "Analyses " : "") + (engine->runsRCmd() ? "RCmder " : "") + (engine->runsUtility() ? "Utilities " : "") ;
	}

	return QVariant();
}

QHash<int, QByteArray> EngineSync::roleNames() const
{
	static bool						set = false;
	static QHash<int, QByteArray> roles = QAbstractListModel::roleNames ();

	if(!set)
	{
		for(const auto & listRole : enginesListRolesToStringMap())
			roles[listRole.first] = tq(listRole.second).toUtf8();

		set = true;
	}

	return roles;
}

size_t EngineSync::maxEngineCount() const
{
	size_t maxEngines = std::max(1, PreferencesModel::prefs()->maxEngines());	
	return maxEngines;
}

void EngineSync::maxEngineCountChanged()
{
	Log::log() << "EngineSync::maxEngineCountChanged called and currently there are #" << _moduleEngines.size() << " while the max we want is: " << maxEngineCount() << std::endl;
	
	//Kill those engines with too high channelnumbers so that we can also destroy the corresponding channels that are no longer needed
	for(size_t e=maxEngineCount(); e<_engines.size(); e++)
	{
		//Pick first one by default
		std::set<EngineRepresentation*> destroyUs;
		
		for(auto * engine : _engines)
			if(engine->channelNumber() == e)
			{
				destroyUs.insert(engine);
				break;
			}
		
		for(auto * engine : destroyUs)
			destroyEngine(engine);
	}

	if(_channels.size() < maxEngineCount())
	{
		size_t startHere = _channels.size();
		_channels.resize(maxEngineCount());

		for(size_t c=startHere; c<_channels.size(); c++)
			_channels[c] = new IPCChannel(_memoryName, c);
	}

	if(_engineStopTimes.size() != maxEngineCount())
	{
		size_t prev = _engineStopTimes.size();
		_engineStopTimes.resize(maxEngineCount());

		for(;prev < _engineStopTimes.size(); prev++)
			_engineStopTimes[prev] = -1;
	}	
}

EngineRepresentation * EngineSync::createNewEngine(bool addToEngines, int overrideChannel)
{
	try
	{
		size_t freeChannel = overrideChannel != -1 ? overrideChannel : 0;

		if(overrideChannel == -1)
		{
			while(!channelFree(freeChannel) && freeChannel < maxEngineCount())
				freeChannel++;

			if(freeChannel > maxEngineCount())
				throw std::runtime_error("createNewEngine but no engines can be started because no channel is free or cooled down...");

			_engineStopTimes[freeChannel] = -1;
		}

		EngineRepresentation	* engine		= new EngineRepresentation(freeChannel, startSlaveProcess(freeChannel), this);
		
		if(addToEngines)
			_engines.insert(engine);

		connect(engine,						&EngineRepresentation::rCodeReturned,					Analyses::analyses(),	&Analyses::rCodeReturned												);
		connect(engine,						&EngineRepresentation::engineTerminated,				this,					&EngineSync::engineTerminated											);
		connect(engine,						&EngineRepresentation::processNewFilterResult,			this,					&EngineSync::processNewFilterResult										);
		connect(engine,						&EngineRepresentation::filterDone,						this,					&EngineSync::filterDone													);
		connect(engine,						&EngineRepresentation::processFilterErrorMsg,			this,					&EngineSync::processFilterErrorMsg										);
		connect(engine,						&EngineRepresentation::columnDataTypeChanged,			this,					&EngineSync::columnDataTypeChanged,				Qt::QueuedConnection	);
		connect(engine,						&EngineRepresentation::computeColumnSucceeded,			this,					&EngineSync::computeColumnSucceeded,			Qt::QueuedConnection	);
		connect(engine,						&EngineRepresentation::computeColumnFailed,				this,					&EngineSync::computeColumnFailed,				Qt::QueuedConnection	);
		connect(engine,						&EngineRepresentation::moduleInstallationFailed,		this,					&EngineSync::moduleInstallationFailed									);
		connect(engine,						&EngineRepresentation::moduleInstallationSucceeded,		this,					&EngineSync::moduleInstallationSucceeded								);
		connect(engine,						&EngineRepresentation::moduleUninstallingFinished,		this,					&EngineSync::moduleUninstallingFinished									);
		connect(engine,						&EngineRepresentation::moduleLoadingSucceeded,			this,					&EngineSync::moduleLoadingSucceeded										);
		connect(engine,						&EngineRepresentation::moduleLoadingFailed,				this,					&EngineSync::moduleLoadingFailed										);
		connect(engine,						&EngineRepresentation::logCfgReplyReceived,				this,					&EngineSync::logCfgReplyReceived										);
		connect(engine,						&EngineRepresentation::plotEditorRefresh,				this,					&EngineSync::plotEditorRefresh											);
		connect(engine,						&EngineRepresentation::requestEngineRestart,			this,					&EngineSync::restartEngineAfterCrash									);
		connect(engine,						&EngineRepresentation::registerForModule,				this,					&EngineSync::registerEngineForModule									);
		connect(engine,						&EngineRepresentation::unregisterForModule,				this,					&EngineSync::unregisterEngineForModule									);
		connect(engine,						&EngineRepresentation::moduleHasEngine,					this,					&EngineSync::moduleHasEngine											);
		connect(engine,						&EngineRepresentation::channelSignal,					this,					&EngineSync::channel,							Qt::DirectConnection	);
		connect(engine,						&EngineRepresentation::stopAndDestroyEngine,			this,					&EngineSync::stopAndDestroyEngine,				Qt::QueuedConnection	);
		connect(engine,						&EngineRepresentation::stopModuleEngine,				this,					&EngineSync::stopModuleEngine											);
		connect(this,						&EngineSync::reloadData,								engine,					&EngineRepresentation::reloadData										);
		connect(this,						&EngineSync::settingsChanged,							engine,					&EngineRepresentation::settingsChanged									);
		connect(Analyses::analyses(),		&Analyses::analysisRemoved,								engine,					&EngineRepresentation::analysisRemoved									);
		connect(PreferencesModel::prefs(),	&PreferencesModel::maxEnginesChanged,					this,					&EngineSync::maxEngineCountChanged,				Qt::DirectConnection	);

		connect(engine,						&EngineRepresentation::stateChanged,					this,					&EngineSync::resetListModel,					Qt::QueuedConnection	);
		connect(engine,						&EngineRepresentation::analysisStatusChanged,			this,					&EngineSync::resetListModel,					Qt::QueuedConnection	);

		resetListModel();

		return engine;

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

	//We create the channels for all engines (and update this whenever maxEngineCountChange() gets called)
	//This avoids any timing problems and boost-shared-memory file allocation mishaps.
	//Also we do not need to recreate and destroy them all the time this way.
	_channels.resize(maxEngineCount());
	for(size_t c=0; c<maxEngineCount(); c++)
		_channels[c] = new IPCChannel(_memoryName, c);

	//Initialize stop times to -1, because we just started
	_engineStopTimes.resize(maxEngineCount());

	for(size_t s=0;s < _engineStopTimes.size(); s++)
		_engineStopTimes[s] = -1;

	//We start with a single engine. Later we can start more if necessary and allowed by the user. This one engine can run filters etc and it can be assigned to a particular module.
	//Once it is assigned to a module it won't be possible to use it for another module until it is restarted.
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
	for(auto * engine : _engines)
		if(engine->killed())
		{
			engine->restartEngine(startSlaveProcess(engine->channelNumber()));
			Log::log() << "restarted engine " << engine->channelNumber() << std::endl;
		}

	logCfgRequest();
}

void EngineSync::restartEngineAfterCrash(EngineRepresentation * engine)
{
	engine->restartEngine(startSlaveProcess(engine->channelNumber()));
	logCfgRequest();
}

void EngineSync::restartKilledAndStoppedEngines()
{
	for(EngineRepresentation * engine : _engines)
		if(engine->killed() || !engine->jaspEngineStillRunning())
			engine->restartEngine(startSlaveProcess(engine->channelNumber()));
	else if(engine->stopped())
			engine->resumeEngine();
}

void EngineSync::shutdownBoredEngines()
{
	std::vector<EngineRepresentation *> boredEngines;
	for (auto engine : _engines)
	{
		engine->processReplies();

		if(
			_engines.count(engine) > 0	&&
			engine->isBored()			&&

			( _engines.size() - boredEngines.size()  > 1 || engine->module() != "") //because it might be better to have an empty engine later in case the user adds something from a different module
		)
		{
		   Log::log() << "Engine #" << engine->channelNumber()  << " had nothing to do for so long it has decided to shutdown." << std::endl;
		   engine->shutEngineDown();
		   boredEngines.push_back(engine);
		}
	}

	for(EngineRepresentation * engine : boredEngines)
		stopAndDestroyEngine(engine);
}

void EngineSync::process()
{
	if(_stopProcessing)	return;

	if(_rCmder)
		_rCmder->processReplies();
	
	restartKilledAndStoppedEngines();
	shutdownBoredEngines();

	for(auto * engine : _engines)
		engine->processReplies();

	if(moduleInstallRunning()) return; //First finish any module install running.

	processReloadData();
	processSettingsChanged();
	processFilterScript();

	if(_filterRunning) return; //Do not do anything else while waiting for a filter to return
	
	processLogCfgRequests();

	if(_engines.size() == 0)
		startExtraEngines();
	
	bool		notEnoughIdlesForScript		=	processScriptQueue();
	stringset	notEnoughIdlesForModule		=	processDynamicModules();
	auto		notEnoughIdlesForAnalysis	=	processAnalysisRequests();
	bool		notEnoughIdles				=	notEnoughIdlesForScript || notEnoughIdlesForModule.size() || notEnoughIdlesForAnalysis.size();
	int			wantThisManyEngines			=	notEnoughIdlesForModule.size();

	if(notEnoughIdles)
		Log::log() << "Not enough idle engines! Need " << (notEnoughIdlesForScript ? " one for script" : "") << (notEnoughIdlesForModule.size() ? std::to_string(notEnoughIdlesForModule.size()) + " for installing modules" : "") <<  (notEnoughIdlesForAnalysis.size() ? std::to_string(notEnoughIdlesForAnalysis.size()) + " for analysis" : "") << ", one will " << ( !anEngineIdleSoon() ? "NOT " : "")  << "be idle soon..." << std::endl;
	
	//First try to find or start some engines specifically for waiting analyses, and we assign them to the module immediately
	if(notEnoughIdlesForAnalysis.size())
	{
		size_t	canStart(enginesStartableCount()),
				startMe (0);

		for(const std::string & modName : notEnoughIdlesForAnalysis)
			if(!notEnoughIdlesForModule.count(modName))
			{
				bool foundAnEngineAnyway = false;
				//Can we use an existing engine?
				for(auto * engine : _engines)
					if(engine->module() == "" && engine->idleSoon())
					{
						registerEngineForModule(engine, modName);
						foundAnEngineAnyway = true;
						break;
					}

				//If that didn't work maybe we can start a new engine?
				if(!foundAnEngineAnyway && aChannelFree() && startMe++ < canStart)
				{
					auto * engine = createNewEngine();
					registerEngineForModule(engine, modName);
				}
				else
					wantThisManyEngines++; //Otherwise just try later with idle killings
			}
	}

	//Maybe some engine is waiting to continue an aborted analysis, let's do it now so that it won't get killed in startExtraEngines
	for(auto * engine : _engines)
		if(engine->idle())
			engine->restartAbortedAnalysis();

	//We might still want some engines and if we can kill some idle ones to make space it ain't bad
	if(wantThisManyEngines)
		startExtraEngines(wantThisManyEngines);
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

	//First we make sure nothing else is running before we ask the engine to run the filter
	if(!_filterRunning)
	{
		Log::log() << "Pausing and resuming engines to make sure nothing else is running when we start the filter." << std::endl;

		pauseEngines(); //Make sure engines pause/stop
		_filterRunning = true;
		resumeEngines();
	}
	else //So previous loop we made sure nothing else is running, and maybe we had to kill an engine to make it understand, followed by a restart. Now we are ready to run the filter
	{
		try
		{
			for (auto *engine : _engines)
				if (engine->idle()  && engine->runsUtility())
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

void EngineSync::processReloadData()
{
	for(auto * engine : _engines)
		if(engine->needsReloadData())
			engine->sendReloadData();
}


bool EngineSync::processScriptQueue()
{
	try
	{
		for(auto * engine : _engines)
		{
			if(engine->idle()  && engine->runsUtility() && _waitingScripts.size() > 0)
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
	
	return _waitingScripts.size() > 0; //Because if there are still scripts waiting we didnt find an idle engine for it
}


stringset EngineSync::processDynamicModules()
{
	using DynMods = Modules::DynamicModules;

	try
	{
		stringset	wantToRunInstall	= DynMods::dynMods()->modulesNeedingPackagesInstalled(),
					stillWantTo			= {};
		
		for(const std::string & mod : wantToRunInstall)
		{
			if(moduleHasEngine(mod))
			{
				auto * engine = _moduleEngines[mod];

				if(engine->analysisInProgress())
					engine->killEngine();

				if(engine->idle())
					engine->runModuleInstallRequestOnProcess(DynMods::dynMods()->getJsonForPackageInstallationRequest(mod));
			}
			else
				for(auto & engine : _engines)
					if(engine->idle() && engine->runsUtility()) //We don't care if the engine is meant for some module or other. We restart afterwards anyway
					{
						registerEngineForModule(engine, mod);
						engine->runModuleInstallRequestOnProcess(DynMods::dynMods()->getJsonForPackageInstallationRequest(mod));
					}
					else
						stillWantTo.insert(mod);
		}
		
		return stillWantTo;
	}
	catch(Modules::ModuleException & e)	{ Log::log() << "Exception thrown in processDynamicModules: " <<  e.what() << std::endl;	}
	catch(std::exception & e)			{ Log::log() << "Exception thrown in processDynamicModules: " << e.what() << std::endl;		}
	catch(...)							{ Log::log() << "Unknown Exception thrown in processDynamicModules..." << std::endl;		}
	
	return {};
}

std::set<std::string> EngineSync::processAnalysisRequests()
{	

	std::set<std::string> modulesNeedingEngines;
	
	for(auto * engine : _engines)
		engine->handleRunningAnalysisStatusChanges();

	Analyses::analyses()->applyToAll([&](Analysis * analysis)
	{
		if(analysis && analysis->shouldRun())
		{
			try
			{
				const std::string modName = analysis->dynamicModule()->name();

				//First check if we already have an engine for this module
				if(moduleHasEngine(modName))
				{
					auto * engine = _moduleEngines[modName];

					if(engine->willProcessAnalysis(analysis))
						engine->runAnalysisOnProcess(analysis);

					else if(engine->stopped())
						startStoppedEngine(engine);

					else if(engine->idle())
					{
						if(!engine->moduleLoaded())
						{
							if(!engine->moduleLoading())
								engine->moduleLoad();
						}
						//else
						// If the engine is being stopped it might be here	throw std::runtime_error("An engine is meant for module " + modName + " but won't process analysis " + analysis->name() + " and is also loaded, which does not make any sense.");
					}
				}
				else
				{
					bool foundOne = false;

					//See if there is an idle engine we can use
					for(auto * engine : _engines)
						if(engine->module() == "" && !foundOne)
							if(engine->idle() && engine->runsAnalysis())
							{
								registerEngineForModule(engine, modName);
								foundOne = true;
							}


					if(!foundOne)
						modulesNeedingEngines.insert(modName);
						
				}

			}
			catch(std::exception & e)	{ Log::log() << "Exception " << e.what() << " thrown in ProcessAnalysisRequests" << std::endl;	}
		}
	});
	
	return modulesNeedingEngines;
}

///Maybe no engines are idle, but if one is initializing or setting up some stuff it'll be so soon. So tell JASP to be patient then.
bool EngineSync::anEngineIdleSoon() const
{
	for(auto * engine : _engines)
		if(engine->idleSoon())
			return true;
	return false;
}

IPCChannel *EngineSync::channel(size_t channelNumber)
{
	if(_rCmderChannel && channelNumber == _rCmderChannel->channelNumber())
		return _rCmderChannel;

	if(channelNumber > _channels.size())
	{
		Log::log() << "IPCChannel requested for channel #" + std::to_string(channelNumber) + " but only " + std::to_string(_channels.size()) + " exist...";
		return nullptr;
	}

	return _channels[channelNumber];
}

size_t EngineSync::enginesIdleSoon() const
{
	size_t num = 0;
	for(auto * engine : _engines)
		if(engine->idleSoon())
			num++;
	
	return num;
}

size_t EngineSync::enginesStartableCount() const
{
	size_t enginesPossible = maxEngineCount() - _engines.size();

	//But perhaps they have to cool down for a bit.

	for(long engineStopTime : _engineStopTimes)
		if(engineStopTime != -1 && ( engineStopTime + ENGINE_COOLDOWN > Utils::currentMillis() ) && enginesPossible > 0)
			enginesPossible--;

	return enginesPossible;
}

bool EngineSync::channelCooledDown(size_t channel) const
{
	return _engineStopTimes[channel] == -1 || _engineStopTimes[channel] + ENGINE_COOLDOWN < Utils::currentMillis();
}

bool EngineSync::channelFree(size_t channel) const
{
	for(auto * engine : _engines)
		if(engine->channelNumber() == channel)
			return false;

	if(!channelCooledDown(channel))
		return false;

	return true;
}

bool EngineSync::aChannelFree() const
{
	for(size_t c=0; c<_channels.size() && c<maxEngineCount(); c++)
		if(channelFree(c))
			return true;
	return false;
}

void EngineSync::startExtraEngines(size_t num)
{
	for(; enginesStartableCount() && num > 0; num--)
		if(aChannelFree())
			createNewEngine();

	if(num)
	{
		Log::log() << "Too many engines running already, perhaps it is time to kill up to " << num << " idle one" << (num == 1 ? "" : "s") << "." << std::endl;
		

		std::vector<std::pair<int, EngineRepresentation *>> idleEngines;

		for(auto * e : _engines)
			if(e->idle() && e->idleFor() > 0)
				idleEngines.push_back(std::make_pair(e->idleFor(), e));

		std::sort(idleEngines.begin(), idleEngines.end(), [](auto & l, auto & r) { return l.first > r.first; }); //longest idle first please

		for(size_t i=0; i<idleEngines.size() && num > 0; i++)
		{
			auto * engine = idleEngines[i].second;
			Log::log() << "Found an idle one, destroying it (" << engine->channelNumber() << "), it was idle for " << idleEngines[i].first << "s." << std::endl;

			stopAndDestroyEngine(engine);
			//createNewEngine(); //Dont do it here, but wait for the next loop, and we will makes ure there is a small builtin delay to avoid boost failing hard on windows
			num--;
		}

		if(num > 0)
			Log::log() << "Still need " << num << ", let's try again later." << std::endl;
	}
}


#ifdef _WIN32 
///Overwrites the PATH with a simple clean one
void EngineSync::fixPATHForWindows(QProcessEnvironment & env)
{
	const QString R_ARCH =
#ifdef _WIN64
		"x64";
#else
		"i386";
#endif
	
	env.insert("PATH", AppDirs::programDir().absolutePath() + ";" + QDir(AppDirs::rHome()).absoluteFilePath("bin") + ";" + QDir(AppDirs::rHome()).absoluteFilePath("bin/" + R_ARCH)); // + rtoolsInPath); 

	Log::log() << "Windows PATH was changed to: '" << env.value("PATH", "???") << "'" << std::endl;
}
#endif 

//Should this function go to EngineRepresentation?
QProcess * EngineSync::startSlaveProcess(int channel)
{
	JASPTIMER_SCOPE(EngineSync::startSlaveProcess);
	QDir programDir			= AppDirs::programDir();
	QString engineExe		= programDir.absoluteFilePath("JASPEngine");
	QProcessEnvironment env = ProcessHelper::getProcessEnvironmentForJaspEngine();
	
#ifdef _WIN32 
	fixPATHForWindows(env);
#endif
	
	env.insert("GITHUB_PAT", PreferencesModel::prefs()->githubPatResolved());

	QStringList args;
	args << QString::number(channel) << QString::number(ProcessInfo::currentPID()) << QString::fromStdString(Log::logFileNameBase) << QString::fromStdString(Log::whereStr());

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

bool EngineSync::moduleInstallRunning() const
{
	for(auto * e : _engines)
		if(e->installingModule())
			return true;
	return false;
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
	_stopProcessing = true; 
	
	auto timeout = QDateTime::currentSecsSinceEpoch() + 10;

	for(EngineRepresentation * e : _engines)
		e->stopEngine();

	while(!allEnginesStopped())
		if(timeout < QDateTime::currentSecsSinceEpoch())
		{
			Log::log() << "Waiting for engine to reply stopRequest took longer than timeout, killing it/them.." << std::endl;
			for(EngineRepresentation * e : _engines)
				if(!e->stopped() && !e->killed())
					e->killEngine();

			break;
		}
		else
			for (auto * engine : _engines)
				engine->processReplies();

	Log::log() << "Engines stopped(/killed)" << std::endl;
}

void EngineSync::pauseEngines(bool unloadData)
{
	JASPTIMER_SCOPE(EngineSync::pauseEngines);

	//make sure we process any received messages first.
	for(auto * engine : _engines)
		engine->processReplies();

	for(EngineRepresentation * e : _engines)
		e->pauseEngine(unloadData);

	long tryTill = Utils::currentSeconds() + ENGINE_KILLTIME; //Ill give the engine 1 sec to respond

	while(!allEnginesPaused() && tryTill >= Utils::currentSeconds())
		for (auto * engine : _engines)
			engine->processReplies();

	for (auto * engine : _engines)
		if(!engine->paused())
			engine->killEngine();
}

void EngineSync::startStoppedEngine(EngineRepresentation * engine)
{
	if(!engine->jaspEngineStillRunning())
		engine->restartEngine(startSlaveProcess(engine->channelNumber()));
	else
		engine->resumeEngine();
}

void EngineSync::resumeEngines()
{
	JASPTIMER_SCOPE(EngineSync::resumeEngines);
	
	for(EngineRepresentation * engine : _engines)
		startStoppedEngine(engine);
	
	_stopProcessing = false;

	while(!allEnginesResumed())
		for (auto * engine : _engines)
			engine->processReplies();
}

bool EngineSync::allEnginesStopped(std::set<EngineRepresentation *> these)
{
	for(auto * engine : these.size() > 0 ? these : _engines)
		if(!engine->stopped())
			return false;
	return true;
}

bool EngineSync::allEnginesPaused(std::set<EngineRepresentation *> these)
{
	for(auto * engine : these.size() > 0 ? these : _engines)
		if(!engine->paused()) //Initializing() is part paused()
			return false;
	return true;
}

bool EngineSync::allEnginesResumed(std::set<EngineRepresentation *> these)
{
	for(auto * engine : these.size() > 0 ? these : _engines)
		if(!engine->resumed())
			return false;
	return true;
}

bool EngineSync::allEnginesInitializing(std::set<EngineRepresentation *> these)
{
	for(auto * engine : these.size() > 0 ? these : _engines)
		if(!engine->initializing())
			return false;
	return true;
}

void EngineSync::enginesPrepareForData()
{
	JASPTIMER_SCOPE(EngineSync::enginesPrepareForData);

	//make sure we process any received messages first.
	for(auto * engine : _engines)
		engine->processReplies();

	std::set<EngineRepresentation *> pauseOrKillThese;

	for(EngineRepresentation * e : _engines)
		if(e->busyWithData())
			pauseOrKillThese.insert(e);

	long tryTill = Utils::currentSeconds() + ENGINE_KILLTIME; //Ill give the engine 1 sec to respond

	while(!allEnginesPaused(pauseOrKillThese) && tryTill >= Utils::currentSeconds())
		for (auto * engine : pauseOrKillThese)
			engine->processReplies();

	for (auto * engine : pauseOrKillThese)
		if(!engine->paused())
			engine->killEngine();
}

void EngineSync::enginesReceiveNewData()
{
	if(_stopProcessing)
		return;
	
	JASPTIMER_SCOPE(EngineSync::enginesReceiveNewData);

	//make sure we process any received messages first.
	for(auto * engine : _engines)
		engine->processReplies();

	for(auto * engine : _engines)
		if(engine->paused())
			engine->resumeEngine();

	for(auto * engine : _engines)
		if(!engine->jaspEngineStillRunning())
			engine->restartEngine(startSlaveProcess(engine->channelNumber()));

	emit reloadData();
}

bool EngineSync::isModuleInstallRequestActive(const QString &moduleName)
{
	for(auto * e : _engines)
		if(e->installingModule() && e->handlingModuleRequest(fq(moduleName)))
			return true;
	return false;
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
		_logCfgRequested.insert(e);
}

void EngineSync::logCfgReplyReceived(EngineRepresentation * engine)
{
	_logCfgRequested.erase(engine);
}

void EngineSync::registerEngineForModule(EngineRepresentation * engine, std::string modName)
{
	if(_moduleEngines.count(modName) > 0 && _moduleEngines[modName] != engine)
		throw std::runtime_error("Trying to register module '" + modName + "' to engine #" +
								 std::to_string(engine->channelNumber()) + " but it is already registered to " +
								 std::to_string(_moduleEngines[modName]->channelNumber()));

	Log::log() << "Registering engine #" << engine->channelNumber() << " for module '" << modName << "'" << std::endl;

	_moduleEngines[modName] = engine;

	engine->setDynamicModule(modName);
}

void EngineSync::unregisterEngineForModule(EngineRepresentation * engine, std::string modName)
{
	if(_moduleEngines.count(modName) > 0 && _moduleEngines[modName] != engine)
		return;

	Log::log() << "Unregistering engine #" << engine->channelNumber() << " for module '" << modName << "'" << std::endl;
	_moduleEngines.erase(modName); //We only erase it when it is the exact same engine + modName combo
	engine->setDynamicModule("");
	//engine->shutEngineDown(); this function is triggered by closing the engine anyway
}

void EngineSync::stopModuleEngine(QString moduleName)
{
	const std::string modName = fq(moduleName);
	if(_moduleEngines.count(modName))
		_moduleEngines[modName]->shutEngineDown();
}

void EngineSync::moduleInstallationFailedHandler(const QString &moduleName, const QString &)
{
	const std::string modName = fq(moduleName);
	if(_moduleEngines.count(modName))
		unregisterEngineForModule(_moduleEngines[modName], modName);
}

void EngineSync::killModuleEngine(Modules::DynamicModule * mod)
{
	if(!_moduleEngines.count(mod->name()))
		return;

	_moduleEngines[mod->name()]->shutEngineDown();
}

void EngineSync::killEngine(int channelNumber)
{
	for(auto * engine : _engines)
		if(engine->channelNumber() == channelNumber)
		{
			if(!engine->killed())
				engine->killEngine();
			return;
		}
}

void EngineSync::processLogCfgRequests()
{
	if (_logCfgRequested.size() == 0)
		return;

	try
	{
		for(auto * engine : _logCfgRequested)
			if(engine->idle())
				engine->sendLogCfg();
	}
	catch (...)
	{
		Log::log() << "Exception thrown in processLogCfgRequests" << std:: endl << std::flush;
	}
}

void EngineSync::cleanUpAfterClose()
{
	//try { stopEngines(); } //Tends to go wrong when the engine was already killed (for instance because it didnt want to pause)
	try {	pauseEngines(); }
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

	try { resumeEngines(); }
	//try { restartEngines(); }
	catch(unexpectedEngineReply e) {}

	resetListModel();
}

std::string	EngineSync::currentStateForDebug() const
{
	try
	{
		std::stringstream out;

		for(auto * engine : _engines)
			try			{ out << engine->currentStateForDebug() << "\n"; }
			catch(...)	{ out << "Something is wrong with engine " << engine->channelNumber() << "...\n"; }

		return out.str();
	}
	catch(...)
	{
		return "EngineSync::currentStateForDebug() did not work...\n";
	}

}

EngineRepresentation *	EngineSync::createRCmdEngine()
{
	if(!_rCmder)
	{
		const size_t rCmdChannelNumber = 12345; //Shouldnt ever crash with _channels

		_rCmderChannel	= new IPCChannel(_memoryName, rCmdChannelNumber);
		_rCmder			= createNewEngine(false, rCmdChannelNumber);

		_rCmder->setRunsAnalysis(	false);
		_rCmder->setRunsUtility(	false);
		_rCmder->setRunsRCmd(		true);

		resetListModel();
	}

	return _rCmder;
}

void EngineSync::destroyEngine(EngineRepresentation * engine)
{
	if(!engine) return;

	const size_t channel = engine->channelNumber();

	if(channel < _engineStopTimes.size())
	{
		_engineStopTimes[channel] = Utils::currentMillis();

		QTimer::singleShot(ENGINE_COOLDOWN / 2, [channel, this]()
		{
			if(_channels.size() > channel) //still there?
				_channels[channel]->findConstructAllAgain();
		});
	}

	if(engine->module() != "")	_moduleEngines.erase(engine->module());
	else
	{
		std::string modName = "";

		for(const auto & nameEngine : _moduleEngines)
			if(nameEngine.second == engine)
				modName = nameEngine.first;

		_moduleEngines.erase(modName);
	}

	_engines.erase(engine);

	delete engine;

	if(_rCmder  == engine)
	{
		_rCmder  = nullptr;

		delete _rCmderChannel;
		_rCmderChannel = nullptr;
	}

	resetListModel();
}

void EngineSync::stopAndDestroyEngine(EngineRepresentation * engine)
{
	engine->shutEngineDown();
	destroyEngine(engine);
}
