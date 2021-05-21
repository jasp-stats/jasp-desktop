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

#ifdef __APPLE__
#include <semaphore.h>
#else
#include <boost/interprocess/sync/named_semaphore.hpp>
#endif

#include <boost/interprocess/sync/interprocess_mutex.hpp>

#include "enginerepresentation.h"

/// EngineSync is responsible for launching the background
/// processes, scheduling analyses, and for sending and
/// receiving communications with the running analyses.
/// It keeps track of which analyses, etc are executing on
/// which background process through the use of EngineRepresentation
/// 
class EngineSync : public QAbstractListModel
{
	Q_OBJECT
	
public:

	EngineSync(QObject *parent);
	~EngineSync();

	void start(int ppi);
	bool allEnginesInitializing(std::set<EngineRepresentation *> these = {}); ///< If `these` isn't filled all engines are checked

	static EngineSync * singleton() { return _singleton; }

	EngineRepresentation *	createNewEngine(bool addToEngines = true, int overrideChannel = -1);
	EngineRepresentation *	createRCmdEngine();

	int						rowCount(const QModelIndex & = QModelIndex())				const override;
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	QHash<int, QByteArray>	roleNames()													const override;


public slots:
	void		destroyEngine(EngineRepresentation * engine);
	void		stopAndDestroyEngine(EngineRepresentation * engine);
	int			sendFilter(		const QString & generatedFilter,	const QString & filter);
	void		sendRCode(		const QString & rCode,				int requestId,					bool whiteListedVersion);
	void		computeColumn(	const QString & columnName,			const QString & computeCode,	columnType columnType);
	void		pauseEngines(bool  unloadData = false);
	void		stopEngines();
	void		resumeEngines();
	void		restartEngines();
	void		startStoppedEngine(EngineRepresentation * engine);
	void		refreshAllPlots();
	void		logCfgRequest();
	void		logToFileChanged(bool) { logCfgRequest(); }
	void		cleanUpAfterClose();
	void		filterDone(int requestID);
	void		haveYouTriedTurningItOffAndOnAgain() { stopEngines(); resumeEngines(); } // https://www.youtube.com/watch?v=DPqdyoTpyEs
	void		killModuleEngine(Modules::DynamicModule * mod);
	void		killEngine(int channelNumber);
	void		enginesPrepareForData();
	void		enginesReceiveNewData();
	bool		isModuleInstallRequestActive(const QString & moduleName);
	std::string	currentStateForDebug() const;
	void		dataModeChanged(bool dataMode);
	

signals:
	void		processNewFilterResult(const std::vector<bool> & filterResult, int requestID);
	void		processFilterErrorMsg(const QString & error, int requestID);
	void		engineTerminated();
	void		filterUpdated(int requestID);
	void		filterErrorTextChanged(const QString & error);

	void		computeColumnSucceeded(			const QString & columnName, const QString & warning, bool dataChanged);
	void		computeColumnFailed(			const QString & columnName, const QString & error);
	void		columnDataTypeChanged(			const QString & columnName);

	void		moduleInstallationSucceeded(	const QString & moduleName);
	void		moduleInstallationFailed(		const QString & moduleName, const QString & errorMessage);
	void		moduleLoadingSucceeded(			const QString & moduleName);
	void		moduleLoadingFailed(			const QString & moduleName, const QString & errorMessage);
	void		moduleUninstallingFinished(		const QString & moduleName);

	void		refreshAllPlotsExcept(const std::set<Analysis*> & inProgress);
	void		plotEditorRefresh();
	void		settingsChanged();
	void		reloadData();

private:
	//These process functions can request a new engine to be started:
	bool		processScriptQueue();
	stringset	processDynamicModules();
	stringset	processAnalysisRequests();	///< Returns modules that still need an engine
	
	void		processLogCfgRequests();
	void		processFilterScript();
	void		processSettingsChanged();
	void		processReloadData();
	
	void		shutdownBoredEngines();
	bool		allEnginesStopped(	std::set<EngineRepresentation *> these = {}); ///< If `these` isn't filled all engines are checked
	bool		allEnginesPaused(	std::set<EngineRepresentation *> these = {}); ///< If `these` isn't filled all engines are checked
	bool		allEnginesResumed(	std::set<EngineRepresentation *> these = {}); ///< If `these` isn't filled all engines are checked
	QProcess*	startSlaveProcess(int channelNumber);

	bool		moduleInstallRunning()				const;
	size_t		enginesStartableCount()				const;
	bool		channelFree(size_t channel)			const;
	bool		aChannelFree()						const;
	bool		channelCooledDown(size_t channel)	const;

#ifdef _WIN32 
	void		fixPATHForWindows(QProcessEnvironment & env);
#endif
	
	size_t		maxEngineCount() const;
	size_t		enginesIdleSoon() const;

private slots:
	void	deleteOrphanedTempFiles();
	void	heartbeatTempFiles();

	void	process();

	void	restartEngineAfterCrash(EngineRepresentation * engine);
	void	restartKilledAndStoppedEngines();


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
	RFilterStore					*	_waitingFilter					= nullptr;
	bool								_filterRunning					= false,
										_stopProcessing					= false;
	int									_filterCurrentRequestID			= 0;
	std::string							_memoryName,
										_engineInfo;

	std::queue<RScriptStore*>			_waitingScripts;
	std::map<std::string,
		EngineRepresentation * >		_moduleEngines;					///< An engine per module active. Engines will be started and closed as needed.
	std::set<EngineRepresentation*>		_engines,						///< All analysis/utility/module engines, excepting _rCmder
										_logCfgRequested;
	std::vector<IPCChannel*>			_channels;						///< Channels are instantiated separately from the engines to avoid boost messing up
	EngineRepresentation			*	_rCmder				= nullptr;	///< For those special occassions where you just want to shout at R in a more personal manner
	IPCChannel						*	_rCmderChannel		= nullptr;	///< The channel for shouting at R in a more personal manner
	std::vector<long>					_engineStopTimes;				///< Here we keep track of how long ago it is an engine shut down, this way we can give it a slight time between closing and starting an engine. To avoid shared memory problems on windows.

};

#endif // ENGINESYNC_H
