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

#ifndef ENGINESYNC_H
#define ENGINESYNC_H

#include <QAbstractListModel>
#include "enginerepresentation.h"
#include <queue>
#include <list>

/// EngineSync is responsible for launching the background
/// processes, scheduling analyses, and for sending and
/// receiving communications with the running analyses.
/// It keeps track of which analyses, etc are executing on
/// which background process through the use of EngineRepresentation
/// 
class EngineSync : public QAbstractListModel
{
	Q_OBJECT
	
	Q_PROPERTY(bool activateUtilEngine	READ activateUtilEngine WRITE setActivateUtilEngine NOTIFY activateUtilEngineChanged)
	
public:
							EngineSync(const EngineSync &)	= delete;
							EngineSync(EngineSync &&)		= delete;
	EngineSync				&operator=(const EngineSync &)	= delete;
	EngineSync				&operator=(EngineSync &&)		= delete;
	
							EngineSync(QObject *parent);
							~EngineSync();
	
	void					start();
	void					killProcessTimer();
	bool					allEnginesInitializing(std::set<EngineRepresentation *> these =	{}); ///< If `these` isn't filled all engines are checked
	
	static EngineSync	*	singleton() { return _singleton; }
	
	EngineRepresentation *	createNewEngine(bool addToEngines = true,  int overrideChannel = -1, bool privileged = false);
	EngineRepresentation *	createRCmdEngine();
	
	int						rowCount(const QModelIndex & = QModelIndex()) const override;
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
	QHash<int, QByteArray>	roleNames() const override;
	
	std::string				currentStateForDebug() const;

	bool					activateUtilEngine() const;
	void					setActivateUtilEngine(bool newActivateUtilEngine);
	
public slots:
	void		destroyEngine(EngineRepresentation * engine);
	void		stopAndDestroyEngine(EngineRepresentation * engine);
	int			sendFilter(			int dataSetId, const QString & generatedFilter,	const QString & filter);
	void		sendFilterByName(	int dataSetId, const QString & name,				const QString & module);
	void		sendRCode(			int dataSetId, const QString & rCode,				int requestId,					bool whiteListedVersion, QString module);
	void		computeColumn(		int dataSetId, const QString & columnName,			const QString & computeCode,	columnType columnType);
	void		computeDataSet(		int dataSetId, const QString & computeCode,			int defaultInputFilterId);
	void		pauseEngines(bool  unloadData = false);
	void		stopEngines();
	void		resumeEngines();
	void		restartEngines();
	void		startStoppedEngine(EngineRepresentation * engine);
	void		restartKilledAndStoppedEngines();
	void		restartAKilledOrStoppedEngine(EngineRepresentation * engine);
	void		refreshAllPlots();
	void		logCfgRequest();
	void		logToFileChanged(bool) { logCfgRequest(); }
	void		cleanRestart();
	void		filterDone(int requestID);
	void		haveYouTriedTurningItOffAndOnAgain() { stopEngines(); resumeEngines(); } // https://www.youtube.com/watch?v=DPqdyoTpyEs
	void		killModuleEngine(Modules::DynamicModule * mod);
	void		killEngine(int channelNumber);
	void		stopOrKillEngine(int channelNumber);
	bool		isModuleInstallRequestActive(const QString & moduleName);
	void		dataModeChanged(bool dataMode);
	

signals:
	void		engineTerminated();
	void		filterUpdated(int requestID);
	void		filterErrorTextChanged(			const QString & error);

	void		moduleInstallationSucceeded(	const QString & moduleName);
	void		moduleInstallationFailed(		const QString & moduleName, const QString & errorMessage);
	void		moduleLoadingSucceeded(			const QString & moduleName);
	void		moduleLoadingFailed(			const QString & moduleName, const QString & errorMessage);
	void		moduleUninstallationSucceeded(	const QString & moduleName);
	void		moduleUninstallationFailed(		const QString & moduleName, const QString & errorMessage);

	void		refreshAllPlotsExcept(const std::set<Analysis*> & inProgress);
	void		plotEditorRefresh();
	void		settingsChanged();
	void		checkDataSetForUpdates();

	void		activateUtilEngineChanged();
	
private:
	//These process functions can request a new engine to be started:
	stringset	processRCodeQueue();
	bool		processComputedColumnQueue();
	bool		processComputedDataSetQueue();
    bool		processDynamicModules();
	stringset	processAnalysisRequests();	///< Returns modules that still need an engine
	
	void		processLogCfgRequests();
	void		processFilterScript();
	void		processSettingsChanged();
	
	void		shutdownBoredEngines();
	bool		allEnginesStopped(	std::set<EngineRepresentation *> these = {}); ///< If `these` isn't filled all engines are checked
	bool		allEnginesPaused(	std::set<EngineRepresentation *> these = {}); ///< If `these` isn't filled all engines are checked
	bool		allEnginesResumed(	std::set<EngineRepresentation *> these = {}); ///< If `these` isn't filled all engines are checked
    QProcess*	startSlaveProcess(int channelNumber, bool privileged = false);

	bool		moduleInstallRunning()				const;
	size_t		enginesStartableCount()				const;
	bool		channelFree(size_t channel)			const;
	bool		aChannelFree()						const;
	bool		channelCooledDown(size_t channel)	const;

	size_t		maxEngineCount() const;
	size_t		enginesIdleSoon() const;

private slots:
	void	deleteOrphanedTempFiles();
	void	heartbeatTempFiles();

	void	process();

	void	restartEngineAfterCrash(EngineRepresentation * engine);


	void	logCfgReplyReceived(		EngineRepresentation * engine);
	void	registerEngineForModule(	EngineRepresentation * engine, std::string modName);
	void	unregisterEngineForModule(	EngineRepresentation * engine, std::string modName);
	void	stopModuleEngine(			QString moduleName);
	void	moduleInstallationFailedHandler(	const QString & moduleName, const QString & );
	
	void	maxEngineCountChanged();
	void	startExtraEngines(size_t num=1);
	bool	anEngineIdleSoon() const;
	bool	moduleHasEngine(const std::string & name) { return _moduleEngines.count(name); }
	void	resetListModel()	{ beginResetModel(); endResetModel(); } // lets keep things easy here, it doesnt have to be highperf

	IPCChannel * channel(size_t channelNumber);

private:
	std::vector<EngineRepresentation *> orderedEngines() const;

private:
	static EngineSync				*	_singleton;
	QTimer							*	_filterRunningResetTimer		= nullptr,
									*	_timerProcess					= nullptr,
									*	_timerBeat						= nullptr;
	///Pending filters, deduplicated per (dataSetId, script): with a per-dataset workspace more than
	///one dataset can request a filter before the single engine slot is free, so a single _waitingFilter
	///would silently drop the previous dataset's filter. Only one is dispatched at a time.
	std::list<RFilterStore*>			_waitingFilters;
	int									_waitingFilterRequestIDCounter	= 0;
	int									_dispatchedFilterRequestID		= -1; //requestId of the filter currently in flight
	bool								_stopProcessing					= false,
										_dataMode						= false,
										_filterRunning					= false,
										_activateUtilEngine				= false;
	std::string							_memoryName,
										_engineInfo;

	std::queue<RScriptStore*>			_waitingScripts;
	std::queue<RComputeColumnStore*>	_waitingCompCols;
	std::queue<RComputeDataSetStore*>	_waitingCompDataSets;
	std::map<std::string,
		EngineRepresentation * >		_moduleEngines;					///< An engine per module active. Engines will be started and closed as needed.
	std::set<EngineRepresentation*>		_engines,						///< All analysis/utility/module engines, excepting _rCmder
										_logCfgRequested;
	std::vector<IPCChannel*>			_channels;						///< Channels are instantiated separately from the engines to avoid boost messing up
	EngineRepresentation			*	_rCmder				= nullptr;	///< For those special occassions where you just want to shout at R in a more personal manner
	IPCChannel						*	_rCmderChannel		= nullptr;	///< The channel for shouting at R in a more personal manner
	std::vector<int64_t>				_engineStopTimes;				///< Here we keep track of how long ago it is an engine shut down, this way we can give it a slight time between closing and starting an engine. To avoid shared memory problems on windows.
};

#endif // ENGINESYNC_H
