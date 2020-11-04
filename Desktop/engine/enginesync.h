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

#ifdef __APPLE__
#include <semaphore.h>
#else
#include <boost/interprocess/sync/named_semaphore.hpp>
#endif

#include <boost/interprocess/sync/interprocess_mutex.hpp>

#include "enginerepresentation.h"

/* EngineSync is responsible for launching the background
 * processes, scheduling analyses, and for sending and
 * receiving communications with the running analyses.
 * It keeps track of which analyses are executing on
 * which background process.
 */
class EngineSync : public QObject
{
	Q_OBJECT
	
public:
	EngineSync(QObject *parent);
	~EngineSync();

	void start(int ppi);
	bool engineStarted()			{ return _engineStarted; }
	bool allEnginesInitializing();

	static EngineSync * singleton() { return _singleton; }

	EngineRepresentation *	createNewEngine();
	EngineRepresentation *	createRCmdEngine();

public slots:
	void		destroyEngine(EngineRepresentation * engine);
	int			sendFilter(		const QString & generatedFilter,	const QString & filter);
	void		sendRCode(		const QString & rCode,				int requestId,					bool whiteListedVersion);
	void		computeColumn(	const QString & columnName,			const QString & computeCode,	columnType columnType);
	void		pause();
	void		resume();
	void		refreshAllPlots();
	void		stopEngines();
	void		logCfgRequest();
	void		logToFileChanged(bool) { logCfgRequest(); }
	void		cleanUpAfterClose();
	void		filterDone(int requestID);
	std::string	currentState() const;
	void		haveYouTriedTurningItOffAndOnAgain() { stopEngines(); restartEngines(); } // https://www.youtube.com/watch?v=DPqdyoTpyEs
	
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

private:
	//These process functions can request a new engine to be started:
	bool		processScriptQueue();
	bool		processDynamicModules();
	bool		processAnalysisRequests();
	
	void		processLogCfgRequests();
	void		processFilterScript();
	void		processSettingsChanged();
	
	bool		allEnginesStopped();
	bool		allEnginesPaused();
	bool		allEnginesResumed();
	QProcess*	startSlaveProcess(int channelNumber);
	void		checkModuleWideCastDone();
	void		resetModuleWideCastVars();
	void		setModuleWideCastVars(Json::Value newVars);
	bool		amICastingAModuleRequestWide()	{ return !_requestWideCastModuleJson.isNull(); }
	size_t		maxEngineCount() const;

private slots:
	void	deleteOrphanedTempFiles();
	void	heartbeatTempFiles();

	void	process();

	void	moduleLoadingFailedHandler(		const QString & moduleName, const QString & errorMessage, int channelID);
	void	moduleLoadingSucceededHandler(	const QString & moduleName, int channelID);
	void	moduleUnloadingFinishedHandler(	const QString & moduleName, int channelID);

	void	restartEngines();
	void	restartEngineAfterCrash(EngineRepresentation * engine);
	void	restartKilledEngines();

	void	logCfgReplyReceived(EngineRepresentation * engine);
	
	void	maxEngineCountChanged();
	void	startExtraEngine();

private:
	static EngineSync				*	_singleton;
	RFilterStore					*	_waitingFilter					= nullptr;

	bool								_engineStarted					= false,
										_filterRunning					= false;
	int									_filterCurrentRequestID			= 0;
	std::string							_memoryName,
										_engineInfo,
										_requestWideCastModuleName		= "";
	Json::Value							_requestWideCastModuleJson		= Json::nullValue;
	std::map<int, std::string>			_requestWideCastModuleResults;

	std::queue<RScriptStore*>			_waitingScripts;
	std::set<EngineRepresentation*>		_workers,		//Your typical run of the mill engine, can do analyses and Rscript/filter stuff
										_rCmders,		//For those special occassions where you just want to shout at R in a more personal manner
										_engines,		//Keeping track of all those engines isn't easy...
										_logCfgRequested;
};

#endif // ENGINESYNC_H
