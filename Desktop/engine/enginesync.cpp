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
#include <QFileInfo>
#include <QFile>
#include <QDir>
#include "log.h"
#include "dirs.h"
#include "utils.h"
#include "timers.h"
#include "tempfiles.h"
#include <json/json.h>
#include "processinfo.h"
#include "qutils.h"
#include "utilities/appdirs.h"
#include "analysis/analyses.h"
#include "gui/preferencesmodel.h"
#include "utilities/processhelper.h"
#include "utilities/wincontainermanager.h"

using namespace boost::interprocess;

EngineSync * EngineSync::_singleton = nullptr;

EngineSync::EngineSync(QObject *parent)
	: QAbstractListModel(parent)
{
	assert(!_singleton);
	_singleton = this;
	
	_filterRunningResetTimer = new QTimer(this);
	_filterRunningResetTimer->setInterval(1000);
	_filterRunningResetTimer->setSingleShot(true);
	
	connect(_filterRunningResetTimer, &QTimer::timeout, this, [&](){ _filterRunning = false; });
	
	using namespace Modules;

	if(Analyses::analyses())
	{
		connect(Analyses::analyses(),		&Analyses::sendRScript,								this,						&EngineSync::sendRCode							);
		connect(Analyses::analyses(),		&Analyses::sendFilterByName,						this,						&EngineSync::sendFilterByName					);
	}
	
	connect(this,						&EngineSync::moduleInstallationFailed,				this,						&EngineSync::moduleInstallationFailedHandler	);
	
	if(DynamicModules::dynMods())
	{
		connect(this,						&EngineSync::moduleInstallationFailed,				DynamicModules::dynMods(),	&DynamicModules::installationPackagesFailed,	Qt::DirectConnection);
		connect(this,						&EngineSync::moduleInstallationSucceeded,			DynamicModules::dynMods(),	&DynamicModules::installationPackagesSucceeded,	Qt::DirectConnection);

		connect(this,						&EngineSync::moduleUninstallationSucceeded,			DynamicModules::dynMods(),	&DynamicModules::unInstallationPackagesSucceeded,	Qt::DirectConnection);
		connect(this,						&EngineSync::moduleUninstallationFailed,			DynamicModules::dynMods(),	&DynamicModules::unInstallationPackagesFailed,		Qt::DirectConnection);
		connect(this,						&EngineSync::moduleUninstallationFailed,			this,						&EngineSync::moduleInstallationFailedHandler	);
	}
	
	if(PreferencesModel::prefs())
	{
		connect(PreferencesModel::prefs(),	&PreferencesModel::plotPPIChanged,					this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::plotBackgroundChanged,			this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::resultFontChanged,				this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::languageCodeChanged,				this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::developerModeChanged,			this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::githubPatCustomChanged,			this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::githubPatUseDefaultChanged,		this,						&EngineSync::settingsChanged					);
	
		connect(PreferencesModel::prefs(),	&PreferencesModel::numDecimalsChanged,				this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::fixedDecimalsChanged,			this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::exactPValuesChanged,				this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::normalizedNotationChanged,		this,						&EngineSync::settingsChanged					);
		connect(PreferencesModel::prefs(),	&PreferencesModel::maxEnginesChanged,					this,					&EngineSync::maxEngineCountChanged,				Qt::DirectConnection	);
	}
	
	// delay start so as not to increase program start up time 10sec is better than 100ms, because they are orphaned anyway
	// Except, that it might somehow cause a crash? If the timer goes off while waiting for a download from OSF than it might remove the files while making them..
	// So lets put it on 500ms...
	QTimer::singleShot(500, this, &EngineSync::deleteOrphanedTempFiles);

	DataSetPackage::pkg()->setEngineSync(this);

	_memoryName = "JASP-IPC-" + std::to_string(ProcessInfo::currentPID());
}

EngineSync::~EngineSync()
{
	if(!_stopProcessing)
	{
		for(EngineRepresentation * engine : _engines)
		{
			if(!engine->stopped())
				engine->killEngine(false);
			delete engine;
		}
	}

	_moduleEngines.clear();
	_engines.clear();

	for(auto* channel : _channels)
		delete channel;
	_channels.clear();

    destroyEngine(_rCmder);
	delete _rCmderChannel;
	_rCmderChannel	= nullptr;
	_rCmder			= nullptr;

	TempFiles::deleteAll();

	_singleton = nullptr;
}


int EngineSync::rowCount(const QModelIndex &) const
{
	return _engines.size() + int(_rCmder != nullptr);
}

std::vector<EngineRepresentation*>  EngineSync::orderedEngines() const
{
	std::vector<EngineRepresentation*> ordered(_engines.begin(), _engines.end());
	
	if(_rCmder)
		ordered.push_back(_rCmder);

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
	case enginesListRoles::loadingProgress:	return engine->loadingProgress();
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
	size_t maxEngines = PreferencesModel::prefs() ? std::max(1, PreferencesModel::prefs()->maxEngines()) : 1;	
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

EngineRepresentation * EngineSync::createNewEngine(bool addToEngines, int overrideChannel, bool privileged)
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

        EngineRepresentation	* engine		= new EngineRepresentation(freeChannel, startSlaveProcess(freeChannel, privileged), this);
        engine->setIsPrivileged(privileged);
		
		if(addToEngines)
			_engines.insert(engine);

		if(Analyses::analyses()) //Could be missing if testing
		{
			connect(engine,					&EngineRepresentation::rCodeReturned,					Analyses::analyses(),	&Analyses::rCodeReturned												);
			connect(engine,					&EngineRepresentation::filterByNameDone,				Analyses::analyses(),	&Analyses::filterByNameDone,						Qt::QueuedConnection	);
			connect(Analyses::analyses(),	&Analyses::analysisRemoved,								engine,					&EngineRepresentation::analysisRemoved									);
		}
		
		connect(engine,						&EngineRepresentation::filterByNameDone,				DataSetPackage::pkg(),	&DataSetPackage::filterByNameDone,					Qt::QueuedConnection	);
		connect(engine,						&EngineRepresentation::engineTerminated,				this,					&EngineSync::engineTerminated												);
		connect(engine,						&EngineRepresentation::filterDone,						this,					&EngineSync::filterDone														);
		connect(engine,						&EngineRepresentation::moduleInstallationFailed,		this,					&EngineSync::moduleInstallationFailed										);
		connect(engine,						&EngineRepresentation::moduleInstallationSucceeded,		this,					&EngineSync::moduleInstallationSucceeded									);
		connect(engine,						&EngineRepresentation::moduleUninstallationSucceeded,	this,					&EngineSync::moduleUninstallationSucceeded									);
		connect(engine,						&EngineRepresentation::moduleUninstallationFailed,		this,					&EngineSync::moduleUninstallationFailed										);
		connect(engine,						&EngineRepresentation::moduleLoadingSucceeded,			this,					&EngineSync::moduleLoadingSucceeded											);
		connect(engine,						&EngineRepresentation::moduleLoadingFailed,				this,					&EngineSync::moduleLoadingFailed											);
		connect(engine,						&EngineRepresentation::logCfgReplyReceived,				this,					&EngineSync::logCfgReplyReceived											);
		connect(engine,						&EngineRepresentation::plotEditorRefresh,				this,					&EngineSync::plotEditorRefresh												);
		connect(engine,						&EngineRepresentation::requestEngineRestartAfterCrash,	this,					&EngineSync::restartEngineAfterCrash,				Qt::QueuedConnection	);
		connect(engine,						&EngineRepresentation::registerForModule,				this,					&EngineSync::registerEngineForModule										);
		connect(engine,						&EngineRepresentation::unregisterForModule,				this,					&EngineSync::unregisterEngineForModule										);
		connect(engine,						&EngineRepresentation::moduleHasEngine,					this,					&EngineSync::moduleHasEngine												);
		connect(engine,						&EngineRepresentation::checkDataSetForUpdates,			this,					&EngineSync::checkDataSetForUpdates											);
		if(Workspace::singleton())
			connect(engine,					&EngineRepresentation::computeColumnSucceeded,			Workspace::singleton(),	&Workspace::computedColumnSucceeded,						Qt::QueuedConnection		);
		if(Workspace::singleton())
			connect(engine,					&EngineRepresentation::computeDataSetSucceeded,			Workspace::singleton(),	&Workspace::computedDataSetSucceeded,						Qt::QueuedConnection		);
		connect(engine,						&EngineRepresentation::channelSignal,					this,					&EngineSync::channel,								Qt::DirectConnection	);
		connect(engine,						&EngineRepresentation::stopAndDestroyEngine,			this,					&EngineSync::stopAndDestroyEngine,					Qt::QueuedConnection	);
		connect(engine,						&EngineRepresentation::stopModuleEngine,				this,					&EngineSync::stopModuleEngine												);
		connect(this,						&EngineSync::settingsChanged,							engine,					&EngineRepresentation::settingsChanged										);
		
		connect(engine,						&EngineRepresentation::stateChanged,					this,					&EngineSync::resetListModel,						Qt::QueuedConnection	);
		connect(engine,						&EngineRepresentation::analysisStatusChanged,			this,					&EngineSync::resetListModel,						Qt::QueuedConnection	);

		resetListModel();

		return engine;

	}
	catch (interprocess_exception & e)
	{
		Log::log()  << "interprocess exception! " << e.what() <<  std::endl;
		throw e;
	}
}

void EngineSync::start()
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

	_timerProcess	= new QTimer(this);
	_timerBeat		= new QTimer(this);

	connect(_timerProcess,	&QTimer::timeout, this, &EngineSync::process,				Qt::DirectConnection);
	connect(_timerBeat,		&QTimer::timeout, this, &EngineSync::heartbeatTempFiles,	Qt::QueuedConnection);

	_timerProcess->start(100);
	_timerBeat->start(50);
}

void EngineSync::killProcessTimer()
{
	//start() may never have run (tests can skip the engines entirely), in which case the timers
	//do not exist and there is nothing to stop.
	if(_timerProcess)
		_timerProcess->stop();
}

void EngineSync::restartEngines()
{
	for(auto * engine : _engines)
		if(engine->killed())
		{
            engine->restartEngine(startSlaveProcess(engine->channelNumber(), engine->isPrivileged()));
			Log::log() << "restarted engine " << engine->channelNumber() << std::endl;
		}

	logCfgRequest();
	
	_stopProcessing = false;
}

void EngineSync::restartEngineAfterCrash(EngineRepresentation * engine)
{
	Log::log() << "restartEngineAfterCrash(" << engine->channelNumber() << ")" << std::endl;
	
    engine->restartEngine(startSlaveProcess(engine->channelNumber(), engine->isPrivileged()));
	logCfgRequest();
}

void EngineSync::restartKilledAndStoppedEngines()
{
	for(EngineRepresentation * engine : _engines)
		restartAKilledOrStoppedEngine(engine);
}

void EngineSync::restartAKilledOrStoppedEngine(EngineRepresentation * engine)
{

	if(engine->killed() || !engine->jaspEngineStillRunning())
        engine->restartEngine(startSlaveProcess(engine->channelNumber(), engine->isPrivileged()));

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

/**
 * @brief EngineSync::process the beating heart of jasp-desktop
 * 
 * This function Handles starting, stopping and handling engines. 
 * It also distributes jobs to them, this can range from filter-code to be run to rscripts for analyses.
 * Also Module load/install requests are sent to engines and of course analyses can be run.
 * 
 * Each engine can be registered for a module, which should b e combined with a module load if rscripts or analyses need to be ran on it.
 * This allows for clean separation of R-libraries per module (as they each get their own engine and thus R)
 * 
 * It gets runs every 50ms, if it can anyway.
 */
void EngineSync::process()
{
	if(_stopProcessing && !_dataMode)
		return;
		
    if(_rCmder)
    {
        restartAKilledOrStoppedEngine(_rCmder);

        _rCmder->processReplies();
        processDynamicModules();


        if(_rCmder->module() != "" && !_rCmder->moduleLoaded() && !_rCmder->moduleLoading())
            _rCmder->moduleLoad();
    }
    else 
	{
		if(_activateUtilEngine)
			createRCmdEngine(); //Dont just create this by default because it causes crashes on waking from long sleeps...
    }
	
	restartKilledAndStoppedEngines();
	shutdownBoredEngines();

	for(auto * engine : _engines)
		engine->processReplies();

	if(moduleInstallRunning()) return; //First finish any module install running.

	processSettingsChanged();
	
	processFilterScript();
		
	processLogCfgRequests();

	if(_stopProcessing || _dataMode || _filterRunning)
	{
		if ((_dataMode) && (processComputedColumnQueue() || processComputedDataSetQueue()))
			startExtraEngines(1);
		return;
	}

	//if(_engines.size() == 0)
	//	startExtraEngines();
	
	//So we try to distribute some work to each engine as below:
	stringset	notEnoughIdlesForScript		=	processRCodeQueue();
	bool		notEnoughIdlesForCompCol	=	processComputedColumnQueue();
	bool		notEnoughIdlesForCompDataSet=	processComputedDataSetQueue();
	auto		notEnoughIdlesForAnalysis	=	processAnalysisRequests();
    bool		notEnoughIdles				=	notEnoughIdlesForCompCol || notEnoughIdlesForCompDataSet || notEnoughIdlesForScript.size() || notEnoughIdlesForAnalysis.size();
	
	// So  right now notEnoughIdles tells us we do not have enough idle engines (or free idle engines anyway)
	// Now we join the set of missing module-engines, or engines registered for a module (and usually with that module loaded unless it is an install request)
    stringset notEnoughIdlesSet(notEnoughIdlesForScript);
	
	int			wantThisManyEngines			=	notEnoughIdlesSet.size();

	if (notEnoughIdlesForCompCol) // Need an engine for a computed column: create one!
		wantThisManyEngines++;
	if (notEnoughIdlesForCompDataSet) // Need an engine for a computed dataset: create one too!
		wantThisManyEngines++;

	if(notEnoughIdles)
        Log::log() << "Not enough idle engines! Need " << (notEnoughIdlesForScript.size() ? " one for script" : "") << (notEnoughIdlesForCompCol ? " one for compcol" : "") << (notEnoughIdlesForCompDataSet ? " one for compdata" : "") << (notEnoughIdlesForAnalysis.size() ? std::to_string(notEnoughIdlesForAnalysis.size()) + " for analysis" : "") << ", one will " << ( !anEngineIdleSoon() ? "NOT " : "")  << "be idle soon..." << std::endl;
	
	//First try to find or start some engines specifically for waiting analyses, and we assign them to the module immediately
	if(notEnoughIdlesForAnalysis.size())
	{
		size_t	canStart(enginesStartableCount()),
				startMe (0);

		for(const std::string & modName : notEnoughIdlesForAnalysis)
			if(!notEnoughIdlesSet.count(modName))
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


	/*//So, in the end all the code above here is a bit complicated and does many things. but...
	// We probably want to have as many engines loaded as allowed. Especially if the dataset is large
	// This will make it seem smoother to the user, because they will have to wait less for data loading
	if(enginesStartableCount() > 0)
		startExtraEngines();*/
	
	//There seem to be some scenarios where engines get stuck in a paused state, this doesn't seem right and if we manage to get all the way down here we can probably try and resume them
	for(auto * engine : _engines)
		if(engine->paused())
			engine->resumeEngine();
}

int EngineSync::sendFilter(int dataSetId, const QString & generatedFilter, const QString & filter)
{
	JASPTIMER_SCOPE(EngineSync::sendFilter);

	//Only the latest request per (dataSetId, script) needs to be kept: drop a pending filter that the
	//new one supersedes so the queue cannot grow unboundedly while engines are busy.
	for (auto it = _waitingFilters.begin(); it != _waitingFilters.end();)
	{
		RFilterStore * w = *it;
		if (w->dataSetId == dataSetId && w->generatedfilter == generatedFilter && w->script == filter)
		{
			delete w;
			it = _waitingFilters.erase(it);
		}
		else
			++it;
	}

	int requestId = ++_waitingFilterRequestIDCounter;
	_waitingFilters.emplace_back(new RFilterStore(dataSetId, generatedFilter, filter, requestId));
	Log::log() << "waiting filter with requestid: " << requestId << " is now:\n" << generatedFilter.toStdString() << "\n" << filter.toStdString() << std::endl;

	return requestId;
}

void EngineSync::sendFilterByName(int dataSetId, const QString & name, const QString & module)
{
	std::queue<RScriptStore *> copyQueue = _waitingScripts;
	
	if(copyQueue.size() > 0)
		for(RScriptStore * script = copyQueue.front(); copyQueue.size() > 0; script = copyQueue.front(), copyQueue.pop())
		{
			//Only deduplicate against actual filter-by-name requests (the queue also holds plain R
			//scripts) and match the dataSetId too: in a multi-dataset workspace two datasets can each
			//have a filter with the same name, and collapsing them would never deliver one dataset's reply.
			if(script->typeScript != engineState::filterByName)
				continue;

			auto * waiting = static_cast<RFilterByNameStore*>(script);

			if(waiting->name == name && waiting->module == module && waiting->dataSetId == dataSetId)
				return;
		}
					
	_waitingScripts.push(new RFilterByNameStore(dataSetId, name, module));
}

void EngineSync::sendRCode(int dataSetId, const QString & rCode, int requestId, bool whiteListedVersion, QString module)
{
	_waitingScripts.push(new RScriptStore(dataSetId, requestId, rCode, module, engineState::rCode, whiteListedVersion));
}

void EngineSync::computeColumn(int dataSetId, const QString & columnName, const QString & computeCode, columnType colType)
{
	//first we remove the previously sent requests for this same column!
	std::queue<RComputeColumnStore*> copiedWaiting(_waitingCompCols);
	_waitingCompCols = std::queue<RComputeColumnStore*>() ;

	while(copiedWaiting.size() > 0)
	{
		RComputeColumnStore * cur = copiedWaiting.front();
		copiedWaiting.pop();

		if(cur->typeScript != engineState::computeColumn || cur->dataSetId != dataSetId || static_cast<RComputeColumnStore*>(cur)->_columnName != columnName)
			_waitingCompCols.push(cur);
		else
			delete cur; //superseded by the new request for the same column
	}

	_waitingCompCols.push(new RComputeColumnStore(dataSetId, columnName, computeCode, colType));
}

void EngineSync::computeDataSet(int dataSetId, const QString & computeCode, int defaultInputFilterId)
{
	//first we remove the previously sent requests for this same dataset!
	std::queue<RComputeDataSetStore*> copiedWaiting(_waitingCompDataSets);
	_waitingCompDataSets = std::queue<RComputeDataSetStore*>();

	while(copiedWaiting.size() > 0)
	{
		RComputeDataSetStore * cur = copiedWaiting.front();
		copiedWaiting.pop();

		if(cur->typeScript != engineState::computeDataSet || cur->dataSetId != dataSetId)
			_waitingCompDataSets.push(cur);
		else
			delete cur; //superseded by the new request for the same dataset
	}

	_waitingCompDataSets.push(new RComputeDataSetStore(dataSetId, computeCode, defaultInputFilterId));
}

void EngineSync::processFilterScript()
{
	if(_waitingFilters.empty())
		return;

	JASPTIMER_SCOPE(EngineSync::processFilterScript);

	//First we make sure nothing else is running before we ask the engine to run the filter
	if(!_engines.size() || (!_dataMode && !_filterRunning))
	{
		pauseEngines();
		_filterRunning = true;
		resumeEngines();
		
		if(!_engines.size())
			createNewEngine();
	}
	else //So previous loop we made sure nothing else is running by switching to data editing mode or not having analyses
	{
		try
		{
			for (auto *engine : _engines)
				if (engine->idle()  && engine->runsUtility())
				{
					RFilterStore * w = _waitingFilters.front();
					_waitingFilters.pop_front();

					_dispatchedFilterRequestID = w->requestId;
					engine->runScriptOnProcess(w); //Copies synchronously; safe to free below.
					delete w; //runScriptOnProcess no longer owns the store (and the old single-var path leaked it)
					return;
				}

		} catch (...){	Log::log() << "Exception sent in processFilterScript" << std::endl;	}
	}
}

void EngineSync::filterDone(int requestID)
{
	if(requestID != _dispatchedFilterRequestID)
		return;

	Log::log() << "Filter with request " << requestID << " done! Starting timer for allowing analyses to run later" << std::endl;
	
	_filterRunningResetTimer->start();
}


void EngineSync::processSettingsChanged()
{
	for(auto * engine : _engines)
		if(engine->shouldSendSettings())
			engine->sendSettings();

	if(_rCmder && _rCmder->shouldSendSettings())
		_rCmder->sendSettings();
}


/**
Checks if the top scriptstruct of the _waitingScripts queue is an rcode script that needs to run. Each of these belongs to a specific module, so it needs to find that engine or start it.

    If there is no engine that is already coupled with the module, then it tries to find an idle engine that is not coupled with any module. If such an engine exists, it loads the module: it cannot run the script yet, because the loading of the module can take time. As this method is called every 50 milliseconds, the script will be run automatically when the module is loaded.
    If an engine exists with the right module and is idle, it runs the script.

If an engine is found that can run the script (immediately, or later when the module is loaded), then this method returns an empty set, else it returns a set with only the module name. This is done in order to facilitate the work of the process() function so that it can aggregate the modules that could not be handled, and starts maybe new engines.
**/
stringset EngineSync::processRCodeQueue()
{
	bool	foundEngine		= false, 
			engineNotIdle	= false;
	
	try
	{
		if(_waitingScripts.size() > 0)
		{
			RScriptStore * waiting = _waitingScripts.front();
			
			if(waiting->typeScript == engineState::rCode || waiting->typeScript == engineState::filterByName)
			{
				const std::string mod = fq(waiting->module);
				
				bool anyEngineCanRunIt = waiting->module == "*";
			
				if(anyEngineCanRunIt)
				{
					for(auto & engine : _engines)
						if(engine->idle())
						{
							foundEngine		= true;
							engine->runScriptOnProcess(waiting);
							break;
						}
				}
				else if(!moduleHasEngine(mod))	
				{
					for(auto & engine : _engines)
						if(engine->idle() && engine->module() == "")
						{
							registerEngineForModule(engine, mod);	
							foundEngine		= true;
							engineNotIdle	= true; //it still needs to load the module
							
							if(!engine->moduleLoaded() && !engine->moduleLoading())
								engine->moduleLoad();
						}
				}
				else 
				{
					foundEngine = true;
					if(!_moduleEngines[mod]->idle())	engineNotIdle = true;
					else								_moduleEngines[mod]->runScriptOnProcess(waiting);
				}
			
				
				if(!foundEngine)
					return { mod };
				
				else if(!engineNotIdle)
				{
					delete waiting;
					_waitingScripts.pop();
				}
			}	
					
		}
	}
	catch(std::exception & e)
	{
		Log::log() << "Exception thrown in processScriptQueue: " << e.what() << std::endl;
	}
	catch(...)
	{
		Log::log() << "Exception thrown in processScriptQueue" << std::endl;
	}
	
	return {};
}

bool EngineSync::processComputedColumnQueue()
{
	bool needEngine = false;
	try
	{
		std::queue<RComputeColumnStore*>	newWaiting;
		
		while(_waitingCompCols.size() > 0)
		{
			RComputeColumnStore * waiting = _waitingCompCols.front();
								
			needEngine = true;
			bool foundOne = false;
			
			for(auto * engine : _engines)
				if(engine->idle()  && engine->runsUtility())
				{
					engine->runScriptOnProcess(waiting);
				
					delete waiting;
					_waitingCompCols.pop();
					foundOne = true;
					needEngine = false;
					break;
				}
		
			if(!foundOne)
			{
				newWaiting.push(waiting);
				_waitingCompCols.pop();
			}
		}
		
		_waitingCompCols = newWaiting;
	}
	catch(...)
	{
		Log::log() << "Exception thrown in processComputedColumnQueue" << std::endl;
	}
	
	return needEngine;
}

bool EngineSync::processComputedDataSetQueue()
{
	//Computed datasets whose input is another computed dataset are held until the producer has
	//finished writing to the shared SQLite; this mirrors the dependency ordering already done for
	//computed columns within a single dataset (see iShouldBeSentAgain / checkForDependents). Also
	//prefer dispatching a dependent to the same engine that processed its input.
	bool needEngine = false;
	try
	{
		std::queue<RComputeDataSetStore*>	newWaiting;
		std::set<int>						dispatchedThisPass;

		while(_waitingCompDataSets.size() > 0)
		{
			RComputeDataSetStore * waiting = _waitingCompDataSets.front();

			bool foundOne = false;

			//Don't dispatch a computed dataset whose input is a computed dataset still being computed
			//or that was dispatched this same pass — the producer may not have flushed its output yet.
			bool holdForDependency = false;
			int inputFilterId = waiting->_defaultInputFilterId;
			if(inputFilterId >= 0 && Workspace::singleton())
			{
				Filter * inputFilter = Workspace::singleton()->filterById(inputFilterId);
				if(inputFilter)
				{
					int inputId = inputFilter->data()->id();

					for(auto * engine : _engines)
						if(engine->isComputingDataSet(inputId))
						{
							holdForDependency = true;
							break;
						}
					if(!holdForDependency)
						holdForDependency = dispatchedThisPass.count(inputId) > 0;
				}
			}

			if(!holdForDependency)
			{
				for(auto * engine : _engines)
					if(engine->idle()  && engine->runsUtility())
					{
						engine->runScriptOnProcess(waiting);
						dispatchedThisPass.insert(waiting->dataSetId);
						delete waiting;
						_waitingCompDataSets.pop();
						foundOne = true;
						break;
					}

				//Only report "need another engine" when we actually tried to dispatch and found none
				//idle; a dataset held for its producer must not count as insufficient engines.
				if(!foundOne)
					needEngine = true;
			}

			if(!foundOne)
			{
				newWaiting.push(waiting);
				_waitingCompDataSets.pop();
			}
		}

		_waitingCompDataSets = newWaiting;
	}
	catch(...)
	{
		Log::log() << "Exception thrown in processComputedDataSetQueue" << std::endl;
	}

	return needEngine;
}


bool EngineSync::processDynamicModules()
{
	if(!DynamicModules::dynMods())
		return {}; //Only for testing!
	
	try
	{
		stringset	wantToRunInstall	= DynamicModules::dynMods()->moduleBundlesNeedingInstall();
		stringset	wantToRunUninstall	= DynamicModules::dynMods()->modulesNeedingUninstall();

        if(_rCmder->installingModule() || _rCmder->unInstallingModule()) //lets only process one dynamic module install/remove at a time for the sake of sanity.
            return {};

        if(wantToRunInstall.size() > 0 || wantToRunUninstall.size() > 0)
		{
            if(_rCmder->idle()) //We don't care if the engine is meant for some module or other. We restart afterwards anyway
            {
                if(wantToRunInstall.size() > 0) {
					_rCmder->runModuleInstallRequestOnProcess(DynamicModules::dynMods()->getJsonForBundleInstallRequest());
                    wantToRunInstall = {};
                }
                else if(wantToRunUninstall.size() > 0) {
					_rCmder->runModuleUnInstallRequestOnProcess(DynamicModules::dynMods()->getJsonForModuleUninstallRequest());
                    wantToRunUninstall = {};
                }
            }
		}

        return !(wantToRunInstall.size() + wantToRunUninstall.size() > 0);
	}
	catch(Modules::ModuleException & e)	{ Log::log() << "Exception thrown in processDynamicModules: " <<  e.what() << std::endl;	}
	catch(std::exception & e)			{ Log::log() << "Exception thrown in processDynamicModules: " << e.what() << std::endl;		}
	catch(...)							{ Log::log() << "Unknown Exception thrown in processDynamicModules..." << std::endl;		}
	
    return false;
}

std::set<std::string> EngineSync::processAnalysisRequests()
{
	if(!Analyses::analyses())
		return {}; //Only for testing!

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

	if(channelNumber >= _channels.size())
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
	for(int64_t engineStopTime : _engineStopTimes)
		if(engineStopTime >= 0 && ( engineStopTime + ENGINE_COOLDOWN > Utils::currentMillis() ) && enginesPossible > 0)
			enginesPossible--;

	return enginesPossible;
}

bool EngineSync::channelCooledDown(size_t channel) const
{
	return _engineStopTimes[channel] < 0 || _engineStopTimes[channel] + ENGINE_COOLDOWN < Utils::currentMillis();
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





//Should this function go to EngineRepresentation?
QProcess * EngineSync::startSlaveProcess(int channel, bool privileged)
{
	JASPTIMER_SCOPE(EngineSync::startSlaveProcess);
	
	Log::log(false) << "\n\n###########################################################################################\n" 
					<< "#######         Engine #" << channel << " (re)started at " << Log::getLocalTime() 
					<< "\n\n###########################################################################################\n" 
					<< std::endl;
	
	QDir programDir			= AppDirs::programDir();
	QString engineExe		= programDir.absoluteFilePath("JASPEngine");
	QProcessEnvironment env = ProcessHelper::getProcessEnvironmentForJaspEngine();

	env.insert("GITHUB_PAT", PreferencesModel::prefs()->githubPatResolved());

	QStringList args;
	args << QString::number(channel) << QString::number(ProcessInfo::currentPID()) << tq(Log::logFileNameBase) << tq(Log::whereStr());

	if(Dirs::reportingDir() != "")
		args << tq(Dirs::reportingDir());

	QProcess *slave = new QProcess(this);
	slave->setProcessChannelMode(QProcess::ForwardedChannels);
	slave->setProcessEnvironment(env);
	slave->setWorkingDirectory(programDir.absolutePath());
	
	EngineSync::channel(channel)->touchHeartbeat();

#ifdef _WIN32
    if(privileged || !WinContainerManager::launchSandboxedEngine(slave, engineExe, args))
		slave->start(engineExe, args);
#else
	slave->start(engineExe, args);
#endif

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
	int64_t timeout = Utils::currentMillis() + ENGINE_KILLTIME;
	
	_stopProcessing = true;

	for(EngineRepresentation * e : _engines)
		e->stopEngine();

	while(!allEnginesStopped())
		if(timeout < Utils::currentMillis())
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

	_stopProcessing = true;

	Log::log() << "EngineSync::pauseEngines()" << std::endl;

	//make sure we process any received messages first.
	for(auto * engine : _engines)
		engine->processReplies();

	for(EngineRepresentation * e : _engines)
		e->pauseEngine(unloadData);

	int64_t tryTill = Utils::currentMillis() + ENGINE_KILLTIME;

	while(!allEnginesPaused() && tryTill >= Utils::currentMillis())
		for (auto * engine : _engines)
			engine->processReplies();

	for (auto * engine : _engines)
		if(!engine->paused())
			engine->killEngine();
}

void EngineSync::startStoppedEngine(EngineRepresentation * engine)
{
	if(!engine->jaspEngineStillRunning())
        engine->restartEngine(startSlaveProcess(engine->channelNumber(), engine->isPrivileged()));
	else
		engine->resumeEngine();
}

void EngineSync::resumeEngines()
{
	JASPTIMER_SCOPE(EngineSync::resumeEngines);

	Log::log() << "EngineSync::resumeEngines()" << std::endl;

	for(EngineRepresentation * engine : _engines)
		startStoppedEngine(engine);

	_stopProcessing = false;
	
	while(!allEnginesResumed())
		for(EngineRepresentation * engine : _engines)
		{
			engine->processReplies();
			if(!engine->jaspEngineStillRunning())
				startStoppedEngine(engine);
		}
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

void EngineSync::dataModeChanged(bool dataMode)
{
	_dataMode = dataMode;
	/*if(!dataMode)
	{
		Log::log() << "Data mode turned off, resuming engines." << std::endl;

		resumeEngines();
	}
	else
	{
		Log::log() << "Data mode turned on,  pausing engines." << std::endl;
		pauseEngines();
	}*/
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

void EngineSync::stopOrKillEngine(int channelNumber)
{
	for(auto * engine : _engines)
		if(engine->channelNumber() == channelNumber)
		{
			if(!engine->stopped())
				engine->stopEngine();
			
			int64_t theTimeIsNow = Utils::currentSeconds();
			
			while(Utils::currentSeconds() - theTimeIsNow < 10 && !engine->stopped())
			{
				engine->processReplies();	
			}
			
			if(!engine->stopped() && !engine->killed())
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

void EngineSync::cleanRestart()
{
	Log::log() << "EngineSync::cleanUpAfterClose() )" << std::endl;
	try { stopEngines(); } //Tends to go wrong when the engine was already killed (for instance because it didnt want to pause)
	//try {	pauseEngines(true); }
	catch(unexpectedEngineReply e) {} // If we are cleaning up after close we can get all sorts of things, lets just ignore them.

	while(_waitingScripts.size() > 0)
	{
		delete _waitingScripts.front();
		_waitingScripts.pop();
	}

	while(_waitingCompCols.size() > 0)
	{
		delete _waitingCompCols.front();
		_waitingCompCols.pop();
	}

	for (RFilterStore * f : _waitingFilters)
		delete f;
	_waitingFilters.clear();
	_dispatchedFilterRequestID = -1;
	_filterRunning = false;
	

	TempFiles::clearSessionDir();

	for(EngineRepresentation * e : _engines)
		e->cleanUpAfterClose(true);

	try { resumeEngines(); }
	//try { restartEngines(); }
	catch(unexpectedEngineReply e) {}

	resetListModel();
	_stopProcessing = false;
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
        _rCmder			= createNewEngine(false, rCmdChannelNumber, true);

		_rCmder->setRunsAnalysis(	true);
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
		
		Log::log() << "EngineSync::destroyEngine _engineStopTimes[" << channel << "] = '"<< _engineStopTimes[channel]  <<"'" << std::endl;

		QTimer::singleShot(ENGINE_COOLDOWN / 2, this, [channel, this]()
		{
			Log::log() << "EngineSync::destroyEngine's singleshot _channels[channel]->findConstructAllAgain() fires." << std::endl;
			if(_channels.size() > channel) //still there?
				_channels[channel]->findConstructAllAgain();
		});
	}

	if(engine != _rCmder)
	{
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
	}

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

bool EngineSync::activateUtilEngine() const
{
	return _activateUtilEngine;
}

void EngineSync::setActivateUtilEngine(bool newActivateUtilEngine)
{
	if (_activateUtilEngine == newActivateUtilEngine)
		return;
	
	_activateUtilEngine = newActivateUtilEngine;
	emit activateUtilEngineChanged();
	
	
	
}
