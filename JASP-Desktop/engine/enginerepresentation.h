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

class EngineRepresentation : public QObject
{
	Q_OBJECT

public:
	EngineRepresentation(IPCChannel * channel, QProcess * slaveProcess, QObject * parent = nullptr);
	~EngineRepresentation();

	void		clearAnalysisInProgress();
	void		setAnalysisInProgress(Analysis* analysis);
	Analysis *	analysisInProgress() const { return _analysisInProgress; }

	bool isIdle() { return _engineState == engineState::idle; }

	void handleRunningAnalysisStatusChanges();

	void runScriptOnProcess(RFilterStore * filterStore);
	void runScriptOnProcess(RScriptStore * scriptStore);
	void runScriptOnProcess(RComputeColumnStore * computeColumnStore);
	void runAnalysisOnProcess(Analysis *analysis);
	void runModuleRequestOnProcess(Json::Value request);

	void stopEngine();
	void pauseEngine();
	void resumeEngine();
	void restartEngine(QProcess * jaspEngineProcess);
	bool paused()		const { return _engineState == engineState::paused;												}
	bool initializing()	const { return _engineState == engineState::initializing;										}
	bool resumed()		const { return _engineState != engineState::paused && _engineState != engineState::resuming;	}
	bool stopped()		const { return _engineState == engineState::stopped;											}

	bool jaspEngineStillRunning() { return  _slaveProcess != nullptr; }

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

	void sendLogCfg();

	size_t channelNumber()								{ return _channel->channelNumber(); }


	void sendString(std::string str);

	int engineChannelID()							{ return _channel->channelNumber(); }

public slots:
	void ppiChanged(int newPPI);
	void imageBackgroundChanged(QString value);
	void analysisRemoved(Analysis * analysis);
	void jaspEngineProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);

signals:
	void engineTerminated();
	void processFilterErrorMsg(			const QString & error, int requestId);
	void processNewFilterResult(		const std::vector<bool> & filterResult, int requestId);
	void computeColumnErrorTextChanged(	const QString & error);

	void rCodeReturned(const QString & result, int requestId);

	void computeColumnSucceeded(		const QString & columnName, const QString & warning, bool dataChanged);
	void computeColumnFailed(			const QString & columnName, const QString & error);

	void moduleInstallationSucceeded(	const QString & moduleName);
	void moduleInstallationFailed(		const QString & moduleName, const QString & errorMessage);
	void moduleLoadingSucceeded(		const QString & moduleName, int channelID);
	void moduleLoadingFailed(			const QString & moduleName, const QString & errorMessage, int channelID);
	void moduleUnloadingFinished(		const QString & moduleName, int channelID);
	void moduleUninstallingFinished(	const QString & moduleName);

	void logCfgReplyReceived(int channelNr);

private:
	void sendPauseEngine();
	void sendStopEngine();
	void rerunRunningAnalysis();
	void setChannel(IPCChannel * channel)			{ _channel = channel; }
	void setSlaveProcess(QProcess * slaveProcess);

private:
	Analysis::Status analysisResultStatusToAnalysStatus(analysisResultStatus result, Analysis * analysis);

	QProcess*	_slaveProcess		= nullptr;
	IPCChannel*	_channel			= nullptr;
	Analysis*	_analysisInProgress = nullptr;
	engineState	_engineState		= engineState::initializing;
	int			_ppi				= 96;
	QString		_imageBackground	= "white";
	bool		_pauseRequested		= false,
				_stopRequested		= false;
};

#endif // ENGINEREPRESENTATION_H
