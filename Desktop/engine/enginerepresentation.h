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

class EngineRepresentation : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		runsAnalysis		READ runsAnalysis		WRITE setRunsAnalysis		NOTIFY runsAnalysisChanged		)
	Q_PROPERTY(bool		runsUtility			READ runsUtility		WRITE setRunsUtility		NOTIFY runsUtilityChanged		)
	Q_PROPERTY(bool		runsRCmd			READ runsRCmd			WRITE setRunsRCmd			NOTIFY runsRCmdChanged			)

public:
	EngineRepresentation(IPCChannel * channel, QProcess * slaveProcess, QObject * parent = nullptr);
	~EngineRepresentation();

	void		cleanUpAfterClose();

	void		clearAnalysisInProgress();
	void		setAnalysisInProgress(Analysis* analysis);
	Analysis *	analysisInProgress() const { return _analysisInProgress; }

	void handleRunningAnalysisStatusChanges();

	void runScriptOnProcess(RFilterStore * filterStore);
	void runScriptOnProcess(RScriptStore * scriptStore);
	void runScriptOnProcess(const QString & rCmdCode);
	void runScriptOnProcess(RComputeColumnStore * computeColumnStore);
	void runAnalysisOnProcess(Analysis *analysis);
	void runModuleInstallRequestOnProcess(Json::Value request);
	void runModuleLoadRequestOnProcess(Json::Value request);
	void sendLogCfg();
	void sendSettings();

	///Kills engine outright by killing process
	void killEngine();
	
	///Try to gracefully shut down this engine so it can be removed
	void shutEngineDown();
	
	///Stop the engine but keep it so it can be restarted easily
	void stopEngine();
	
	///Tell the engine it shouldn't be doing anything else, aka pause.
	void pauseEngine(bool unloadData = false);
	
	///Tell engine to resume running
	void resumeEngine();
	
	///Start engine again in jaspEngineProcess
	void restartEngine(QProcess * jaspEngineProcess);
	bool resumed()				const { return _engineState != engineState::resuming && !paused() && !initializing();	}
	bool paused()				const { return _engineState == engineState::paused || initializing();					}
	bool initializing()			const { return _engineState == engineState::initializing;								}
	bool stopped()				const { return _engineState == engineState::stopped;									}
	bool killed()				const { return _engineState == engineState::killed;										}
	bool idle()					const { return _engineState == engineState::idle;										}
	bool idleSoon()				const;
	bool shouldSendSettings()	const { return idle() && _settingsChanged;												}
	bool runsAnalysis()			const { return _runsAnalysis;															}
	bool runsUtility()			const { return _runsUtility;															}
	bool runsRCmd()				const { return _runsRCmd;																}
	bool isBored()			const;

	bool jaspEngineStillRunning() { return  _slaveProcess != nullptr && !killed(); }

	void processReplies();
	void restartAbortedAnalysis();
	void checkIfExpectedReplyType(engineState expected) { unexpectedEngineReply::checkIfExpected(expected, _engineState, channelNumber()); }
	bool willProcessAnalysis(Analysis * analysis) const;

	size_t	channelNumber()		const { return _channel->channelNumber(); }

	std::string currentStateForDebug() const;	
	
protected:
	void processRCodeReply(			Json::Value & json);
	void processFilterReply(		Json::Value & json);
	void processAnalysisReply(		Json::Value & json);
	void processComputeColumnReply(	Json::Value & json);
	void processModuleRequestReply(	Json::Value & json);
	void processEnginePausedReply();
	void processEngineStoppedReply();
	void processEngineResumedReply();
	void processLogCfgReply();
	void processSettingsReply();
	
	void sendString(std::string str);

public slots:
	void analysisRemoved(Analysis * analysis);
	void processFinished(int exitCode, QProcess::ExitStatus exitStatus);
	void settingsChanged();
	void setRunsAnalysis(	bool runsAnalysis);
	void setRunsUtility(	bool runsUtility);
	void setRunsRCmd(		bool runsRCmd);

signals:
	void loadAllActiveModules();
	void engineTerminated();
	void filterDone(															int requestID);
	void processFilterErrorMsg(			const QString & error,					int requestId = -1);
	void processNewFilterResult(		const std::vector<bool> & filterResult, int requestId);
	void computeColumnErrorTextChanged(	const QString & error);

	void rCodeReturned(					const QString & result, int requestId	);
	void rCodeReturnedLog(				const QString & log						);

	void computeColumnSucceeded(		const QString & columnName, const QString & warning, bool dataChanged);
	void computeColumnFailed(			const QString & columnName, const QString & error);
	void columnDataTypeChanged(			const QString & columnName);

	void moduleInstallationSucceeded(	const QString & moduleName);
	void moduleInstallationFailed(		const QString & moduleName, const QString & errorMessage);
	void moduleLoadingSucceeded(		const QString & moduleName, int channelID);
	void moduleLoadingFailed(			const QString & moduleName, const QString & errorMessage, int channelID);
	void moduleUnloadingFinished(		const QString & moduleName, int channelID);
	void moduleUninstallingFinished(	const QString & moduleName);

	void logCfgReplyReceived(	EngineRepresentation * engine);
	void requestEngineRestart(	EngineRepresentation * engine);
	void plotEditorRefresh();
	void runsAnalysisChanged(	bool runsAnalysis);
	void runsUtilityChanged(	bool runsUtility);
	void runsRCmdChanged(		bool runsRCmd);

private:
	void sendPauseEngine();
	void sendStopEngine();
	void setChannel(IPCChannel * channel)			{ _channel = channel; }
	void setSlaveProcess(QProcess * slaveProcess);
	void checkForComputedColumns(const Json::Value & results);
	void handleEngineCrash();
	void abortAnalysisInProgress(bool restartAfterwards);
	void addSettingsToJson(Json::Value & msg);

private:
	static Analysis::Status analysisResultStatusToAnalysStatus(analysisResultStatus result);

	engineState		_engineState		= engineState::initializing; // The representation of whatever state the actual engine is supposed to be in.
	QProcess	*	_slaveProcess		= nullptr;
	IPCChannel	*	_channel			= nullptr;
	Analysis	*	_analysisInProgress = nullptr,
				*	_analysisAborted	= nullptr;	//To make sure we know that the response we got was from this aborted analysis or not
	int				_idRemovedAnalysis	= -1,		//If the analysis was deleted we should ignore its results
					_lastRequestId		= -1,		//for R code requests from qml components, so that we can send it back to the right element
					_abortTime			= -1,		//When did we tell the analysis to abort? So that we can kill it if it takes too long
					_idleStartSecs		= -1;
	bool			_pauseRequested		= false,	//should tell the engine to pause as soon as possible
					_stopRequested		= false,	//should tell the engine to stop as soon as possible
					_slaveCrashed		= false,	//My slave crashed
					_settingsChanged	= true,		//Some setting changed and we should send these new settings asap
					_abortAndRestart	= false,	//abort and restart an analysis
					_runsAnalysis		= true,		//is this engine meant for running analyses?
					_runsUtility		= true,		//is this engine meant for running filters, installing modules or running R Code (not the r prompt though)
					_runsRCmd			= false,	//is this engine meant for the R prompt?
					_removeEngine		= false,
					_pauseUnloadData	= false;
	std::string		_lastCompColName	= "???";

	QMetaObject::Connection	_slaveFinishedConnection;


};

#endif // ENGINEREPRESENTATION_H
