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
	EngineSync(Analyses *analyses, DataSetPackage *package, DynamicModules *dynamicModules, QObject *parent);
	~EngineSync();

	void start(int ppi);
	bool engineStarted()			{ return _engineStarted; }

public slots:
	void sendFilter(	const QString & generatedFilter,	const QString & filter,			int requestID);
	void sendRCode(		const QString & rCode,				int requestId);
	void computeColumn(	const QString & columnName,			const QString & computeCode,	Column::ColumnType columnType);
	void pause();
	void resume();
	void refreshAllPlots(int = 0);
	void stopEngines();

	
signals:
	void processNewFilterResult(const std::vector<bool> & filterResult, int requestID);
	void processFilterErrorMsg(const QString & error, int requestID);
	void engineTerminated();
	void filterUpdated(int requestID);
	void filterErrorTextChanged(const QString & error);

	void ppiChanged(int newPPI);
	void imageBackgroundChanged(const QString & value);

	void computeColumnSucceeded(		const std::string & columnName, const std::string & warning, bool dataChanged);
	void computeColumnFailed(			const std::string & columnName, const std::string & error);

	void moduleInstallationSucceeded(	const std::string & moduleName);
	void moduleInstallationFailed(		const std::string & moduleName, const std::string & errorMessage);
	void moduleLoadingSucceeded(		const std::string & moduleName);
	void moduleLoadingFailed(			const std::string & moduleName, const std::string & errorMessage);
	void moduleUninstallingFinished(	const std::string & moduleName);

	void refreshAllPlotsExcept(const std::set<Analysis*> & inProgress);

private:
	bool		idleEngineAvailable();
	bool		allEnginesStopped();
	bool		allEnginesPaused();
	bool		allEnginesResumed();
	QProcess*	startSlaveProcess(int no);
	void		processScriptQueue();
	void		processDynamicModules();
	void		checkModuleWideCastDone();
	void		resetModuleWideCastVars();
	void		setModuleWideCastVars(Json::Value newVars);
	bool		amICastingAModuleRequestWide()	{ return !_requestWideCastModuleJson.isNull(); }

private slots:
	void ProcessAnalysisRequests();
	void deleteOrphanedTempFiles();
	void heartbeatTempFiles();

	void process();

	void subProcessStarted();
	void subProcessError(QProcess::ProcessError error);
	void subprocessFinished(int exitCode, QProcess::ExitStatus exitStatus);

	void moduleLoadingFailedHandler(		const std::string & moduleName, const std::string & errorMessage, int channelID);
	void moduleLoadingSucceededHandler(		const std::string & moduleName, int channelID);
	void moduleUnloadingFinishedHandler(	const std::string & moduleName, int channelID);

	void restartEngines();

private:
	Analyses		*_analyses;
	bool			_engineStarted = false;
	DataSetPackage	*_package;
	DynamicModules	*_dynamicModules = nullptr;;

	std::queue<RScriptStore*>			_waitingScripts;
	std::vector<EngineRepresentation*>	_engines;
	RFilterStore						*_waitingFilter = nullptr;

	std::string _memoryName,
				_engineInfo;

	std::string					_requestWideCastModuleName	= "";
	Json::Value					_requestWideCastModuleJson	= Json::nullValue;
	std::map<int, std::string>	_requestWideCastModuleResults;
};

#endif // ENGINESYNC_H
