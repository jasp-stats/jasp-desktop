#ifndef ENGINEREPRESENTATION_H
#define ENGINEREPRESENTATION_H

#include <QObject>
#include <QProcess>
#include <QTimer>
#include <vector>

#include "analysis/options/options.h"
#include "analysis/analysis.h"
#include "analysis/analyses.h"
#include "ipcchannel.h"
#include "data/datasetpackage.h"
#include <queue>
#include "enginedefinitions.h"
#include "rscriptstore.h"
#include "modules/dynamicmodules.h"

//How many seconds do we wait for an engine to be killed if it gets stuck in some analysis?
#define KILLTIME 2

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
	void runModuleRequestOnProcess(Json::Value request);
	void sendLogCfg();
	void sendSettings();

	void killEngine(bool disconnectFinished = false);
	void stopEngine();
	void pauseEngine();
	void resumeEngine();
	void restartEngine(QProcess * jaspEngineProcess);
	bool resumed()				const { return _engineState != engineState::resuming && !paused() && !initializing();	}
	bool paused()				const { return _engineState == engineState::paused || initializing();					}
	bool initializing()			const { return _engineState == engineState::initializing;								}
	bool stopped()				const { return _engineState == engineState::stopped;									}
	bool killed()				const { return _engineState == engineState::killed;										}
	bool idle()					const { return _engineState == engineState::idle;										}
	bool shouldSendSettings()	const { return idle() && _settingsChanged;												}
	bool runsAnalysis()			const { return _runsAnalysis;															}
	bool runsUtility()			const { return _runsUtility;															}
	bool runsRCmd()				const { return _runsRCmd;																}

	bool jaspEngineStillRunning() { return  _slaveProcess != nullptr && !killed(); }

	void process();
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
	void restartAbortedAnalysis();

	void checkIfExpectedReplyType(engineState expected) { unexpectedEngineReply::checkIfExpected(expected, _engineState, channelNumber()); }
	bool willProcessAnalysis(const Analysis * analysis) const;

	size_t	channelNumber()		const { return _channel->channelNumber(); }

	void sendString(std::string str);

	std::string currentState() const;


public slots:
	void analysisRemoved(Analysis * analysis);
	void processFinished(int exitCode, QProcess::ExitStatus exitStatus);
	void settingsChanged();
	void setRunsAnalysis(	bool runsAnalysis);
	void setRunsUtility(	bool runsUtility);
	void setRunsRCmd(		bool runsRCmd);

signals:
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

	void logCfgReplyReceived(int channelNr);
	void plotEditorRefresh();
	void requestEngineRestart(int channelNr);
	void runsAnalysisChanged(bool runsAnalysis);
	void runsUtilityChanged(bool runsUtility);
	void runsRCmdChanged(bool runsRCmd);

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

	QProcess	*	_slaveProcess		= nullptr;
	IPCChannel	*	_channel			= nullptr;
	Analysis	*	_analysisInProgress = nullptr,
				*	_analysisAborted	= nullptr;
	engineState		_engineState		= engineState::initializing;
	int				_idRemovedAnalysis	= -1,
					_lastRequestId		= -1,
					_abortTime			= -1;
	bool			_pauseRequested		= false,
					_stopRequested		= false,
					_slaveCrashed		= false,
					_settingsChanged	= true,
					_abortAndRestart	= false;
	std::string		_lastCompColName	= "???";


	bool _runsAnalysis;
	bool _runsUtility;
	bool _runsRCmd;
};

#endif // ENGINEREPRESENTATION_H
