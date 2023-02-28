#ifndef ENGINEREPRESENTATION_H
#define ENGINEREPRESENTATION_H

#include <QObject>
#include <QProcess>
#include <QTimer>
#include <vector>

#include "analysis/analysis.h"
#include "analysis/analyses.h"
#include "ipcchannel.h"
#include "data/datasetpackage.h"
#include <queue>
#include "enginedefinitions.h"
#include "rscriptstore.h"
#include "modules/dynamicmodules.h"

///
/// Keeps track of the state of a single Engine process (JASPEngine)
/// Tracks it through knowing the state, aka is it running an analysis, a filter or something else
/// Also handles pausing, resuming, stopping and restarting them
class EngineRepresentation : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool			runsAnalysis		READ runsAnalysis		WRITE setRunsAnalysis		NOTIFY runsAnalysisChanged		)
	Q_PROPERTY(bool			runsUtility			READ runsUtility		WRITE setRunsUtility		NOTIFY runsUtilityChanged		)
	Q_PROPERTY(bool			runsRCmd			READ runsRCmd			WRITE setRunsRCmd			NOTIFY runsRCmdChanged			)
	Q_PROPERTY(engineState	state				READ state				WRITE setState				NOTIFY stateChanged				)
	Q_PROPERTY(QString		analysisStatus		READ analysisStatus									NOTIFY analysisStatusChanged	)	//For updates to EngineSync' list capabilities
	Q_PROPERTY(QString		module				READ moduleQ										NOTIFY moduleChanged			)


public:
					EngineRepresentation(size_t channelNumber, QProcess * slaveProcess, QObject * parent = nullptr);
					~EngineRepresentation();

	void			cleanUpAfterClose(bool forgetAnalyses = false);

	void			clearAnalysisInProgress();
	void			setAnalysisInProgress(Analysis* analysis);
	Analysis *		analysisInProgress() const { return _analysisInProgress; }

	void			handleRunningAnalysisStatusChanges();

	void			runScriptOnProcess(RFilterStore * filterStore);
	void			runScriptOnProcess(RScriptStore * scriptStore);
	void			runScriptOnProcess(const QString & rCmdCode);
	void			runScriptOnProcess(RComputeColumnStore * computeColumnStore);
	void			runAnalysisOnProcess(Analysis *analysis);
	void			runModuleInstallRequestOnProcess(Json::Value request);
	void			runModuleLoadRequestOnProcess(Json::Value request);
	void			sendLogCfg();
	void			sendSettings();
	void			sendReloadData();

	///Kills engine outright by killing process
	void 			killEngine();
	
	///Try to gracefully shut down this engine so it can be removed
	void 			shutEngineDown();
	
	///Stop the engine but keep it so it can be restarted easily
	void 			stopEngine();
	
	///Tell the engine it shouldn't be doing anything else, aka pause.
	void 			pauseEngine(bool unloadData = false);
	
	///Tell engine to resume running
	void 			resumeEngine(bool setResuming = true);
	
	///Start engine again in jaspEngineProcess
	void 			restartEngine(QProcess * jaspEngineProcess);

	bool			handlingModuleRequest(const std::string & moduleName) const { return _requestModName == moduleName && (installingModule() || moduleLoading()); }

	engineState		state()		const { return _engineState; }

	bool			resumed()				const { return _engineState != engineState::resuming && !paused() && !initializing();	}
	bool			paused()				const { return _engineState == engineState::paused || initializing();					}
	bool			initializing()			const { return _engineState == engineState::initializing;								}
	bool			stopped()				const { return _engineState == engineState::stopped;									}
	bool			killed()				const { return _engineState == engineState::killed;										}
	bool			idle()					const { return _engineState == engineState::idle;										}
	bool			installingModule()		const { return _engineState == engineState::moduleInstallRequest;						}
	bool			moduleLoading()			const { return _engineState == engineState::moduleLoadRequest;							}
	bool			idleSoon()				const;
	bool			shouldSendSettings()	const { return idle() && _settingsChanged;												}
	bool			runsAnalysis()			const { return _runsAnalysis;															}
	bool			runsUtility()			const { return _runsUtility;															}
	bool			runsRCmd()				const { return _runsRCmd;																}
	bool			isBored()				const;
	bool			busyWithData()			const;
	bool			needsReloadData()		const { return idle() && _reloadData; }
	bool			moduleLoaded()			const { return _moduleLoaded; }

	///How many seconds has this engine been idle?
	int				idleFor() const;

	bool			jaspEngineStillRunning() { return  _slaveProcess != nullptr && !killed() && !stopped(); }

	void			processReplies();
	void			restartAbortedAnalysis();
	void			checkIfExpectedReplyType(engineState expected) { unexpectedEngineReply::checkIfExpected(expected, _engineState, channelNumber()); }
	bool			willProcessAnalysis(Analysis * analysis);

	size_t			channelNumber()		const { return _channelNumber; }

	std::string		currentStateForDebug()	const;
	std::string		module()				const { return _dynModName;		}
	QString			moduleQ()				const { return tq(_dynModName);	}
	std::string		moduleRequested()		const { return _requestModName; }
	void			moduleLoad();

	void			setState(engineState newState);

	const QString	analysisStatus() const;


protected:
	void			processRCodeReply(			Json::Value & json);
	void			processFilterReply(		Json::Value & json);
	void			processAnalysisReply(		Json::Value & json);
	void			processComputeColumnReply(	Json::Value & json);
	void			processModuleRequestReply(	Json::Value & json);
	void			processReloadDataReply()							{ _reloadData = false; setState(engineState::idle); }
	void			processEnginePausedReply();
	void			processEngineStoppedReply();
	void			processEngineResumedReply();
	void			processLogCfgReply();
	void			processSettingsReply();

	void			sendString(std::string str);

public slots:
	void			analysisRemoved(Analysis * analysis);
	void			processFinished(int exitCode = 0, QProcess::ExitStatus exitStatus = QProcess::ExitStatus::NormalExit);
	void			settingsChanged();
	void			setRunsAnalysis(	bool runsAnalysis);
	void			setRunsUtility(	bool runsUtility);
	void			setRunsRCmd(		bool runsRCmd);

	void			setDynamicModule(const std::string & dynamicModule);
	void			reloadData() { _reloadData = true; }

signals:
	void			engineTerminated();
	void			filterDone(															int requestID);
	void			processFilterErrorMsg(			const QString & error,					int requestId = -1);
	void			processNewFilterResult(			const std::vector<bool> & filterResult, int requestId);
	void			computeColumnErrorTextChanged(	const QString & error);

	void			rCodeReturned(					const QString & result, int requestId, bool hasError	);
	void			rCodeReturnedLog(				const QString & log, bool hasError						);

	void			computeColumnSucceeded(			const QString & columnName, const QString & warning, bool dataChanged);
	void			computeColumnFailed(			const QString & columnName, const QString & error);
	void			columnDataTypeChanged(			const QString & columnName);

	void			moduleInstallationSucceeded(	const QString & moduleName);
	void			moduleInstallationFailed(		const QString & moduleName, const QString & errorMessage);
	void			moduleLoadingSucceeded(			const QString & moduleName, int channelID);
	void			moduleLoadingFailed(			const QString & moduleName, const QString & errorMessage, int channelID);
	void			moduleUnloadingFinished(		const QString & moduleName, int channelID);
	void			moduleUninstallingFinished(		const QString & moduleName);

	void			logCfgReplyReceived(			EngineRepresentation * engine);
	void			requestEngineRestartAfterCrash(	EngineRepresentation * engine);
	void			registerForModule(				EngineRepresentation * engine, std::string modName);
	void			unregisterForModule(			EngineRepresentation * engine, std::string modName);
	void			stopModuleEngine(				QString moduleName);
	void			stopAndDestroyEngine(			EngineRepresentation * e);
	void			plotEditorRefresh();
	void			runsAnalysisChanged(	bool runsAnalysis);
	void			runsUtilityChanged(	bool runsUtility);
	void			runsRCmdChanged(		bool runsRCmd);

	bool			moduleHasEngine(const std::string & name);

	void			stateChanged();
	void			analysisStatusChanged();
	void			moduleChanged();

	IPCChannel	*	channelSignal(size_t channelNumber);



private:
	void			sendPauseEngine();
	void			sendStopEngine();
	void			setSlaveProcess(QProcess * slaveProcess);
	void			checkForComputedColumns(const Json::Value & results);
	void			handleEngineCrash();
	void			abortAnalysisInProgress(bool restartAfterwards);
	void			addSettingsToJson(Json::Value & msg);

	IPCChannel	*	channel() { return emit channelSignal(_channelNumber); }


private:
	static Analysis::Status analysisResultStatusToAnalysStatus(analysisResultStatus result);

	size_t			_channelNumber		= 0;
	engineState		_engineState		= engineState::initializing; // The representation of whatever state the actual engine is supposed to be in.
	QProcess	*	_slaveProcess		= nullptr;
	Analysis	*	_analysisInProgress = nullptr,
				*	_analysisAborted	= nullptr;	///<To make sure we know that the response we got was from this aborted analysis or not
	int				_idRemovedAnalysis	= -1,		///<If the analysis was deleted we should ignore its results
					_lastRequestId		= -1,		///<for R code requests from qml components, so that we can send it back to the right element
					_abortTime			= -1,		///<When did we tell the analysis to abort? So that we can kill it if it takes too long
					_idleStartSecs		= -1;
	bool			_pauseRequested		= false,	///<should tell the engine to pause as soon as possible
					_stopRequested		= false,	///<should tell the engine to stop as soon as possible
					_slaveCrashed		= false,	///<My slave crashed
					_settingsChanged	= true,		///<Some setting changed and we should send these new settings asap
					_abortAndRestart	= false,	///<abort and restart an analysis
					_runsAnalysis		= true,		///<is this engine meant for running analyses?
					_runsUtility		= true,		///<is this engine meant for running filters, installing modules or running R Code (not the r prompt though)
					_runsRCmd			= false,	///<is this engine meant for the R prompt?
					_removeEngine		= false,
					_pauseUnloadData	= false,
					_reloadData			= false,	///<when the idle is engine and this true, it should reload the data
					_moduleLoaded		= false;	///<If _dynModName is set but this is false the engine should still load the module.
	std::string		_lastCompColName	= "???",
					_dynModName			= "",		///<If filled: refers to the particular dynamic module this engine was meant for.
					_requestModName		= "";		///<To keep track of which engine is handling a request for a module

	QMetaObject::Connection	_slaveFinishedConnection,
							_analysisInProgressStatusConnection;

};

#endif // ENGINEREPRESENTATION_H
