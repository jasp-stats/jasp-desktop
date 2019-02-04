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
	bool paused()	const { return _engineState == engineState::paused;												}
	bool resumed()	const { return _engineState != engineState::paused && _engineState != engineState::resuming;	}
	bool stopped()  const { return _engineState == engineState::stopped;											}

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


	int channelNumber()								{ return _channel->channelNumber(); }


	void sendString(std::string str)
	{
#ifdef PRINT_ENGINE_MESSAGES
		std::cout << "sending to jaspEngine: " << str << "\n" << std::endl;
#endif
		_channel->send(str);
	}

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

	void computeColumnSucceeded(		const std::string & columnName, const std::string & warning, bool dataChanged);
	void computeColumnFailed(			const std::string & columnName, const std::string & error);

	void moduleInstallationSucceeded(	const std::string & moduleName);
	void moduleInstallationFailed(		const std::string & moduleName, const std::string & errorMessage);
	void moduleLoadingSucceeded(		const std::string & moduleName, int channelID);
	void moduleLoadingFailed(			const std::string & moduleName, const std::string & errorMessage, int channelID);
	void moduleUnloadingFinished(		const std::string & moduleName, int channelID);
	void moduleUninstallingFinished(	const std::string & moduleName);

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
	engineState	_engineState		= engineState::idle;
	int			_ppi				= 96;
	QString		_imageBackground	= "white";
	bool		_pauseRequested		= false,
				_stopRequested		= false;
};

#endif // ENGINEREPRESENTATION_H
