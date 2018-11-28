#ifndef ENGINEREPRESENTATION_H
#define ENGINEREPRESENTATION_H

#include <QObject>
#include <QProcess>
#include <QTimer>
#include <vector>

#include "options/options.h"
#include "analysis.h"
#include "analyses.h"
#include "ipcchannel.h"
#include "datasetpackage.h"
#include <queue>
#include "enginedefinitions.h"
#include "rscriptstore.h"

class EngineRepresentation : public QObject
{
	Q_OBJECT

public:
	EngineRepresentation(IPCChannel * channel, QProcess * slaveProcess, QObject * parent = NULL);
	~EngineRepresentation();
	void clearAnalysisInProgress();
	void setAnalysisInProgress(Analysis* analysis);

	bool isIdle() { return _engineState == engineState::idle; }

	void handleRunningAnalysisStatusChanges();

	void runScriptOnProcess(RFilterStore * filterStore);
	void runScriptOnProcess(RScriptStore * scriptStore);
	void runScriptOnProcess(RComputeColumnStore * computeColumnStore);
	void runAnalysisOnProcess(Analysis *analysis);
	void terminateJaspEngine();

	void pauseEngine();
	void resumeEngine();
	bool paused()  const { return _engineState == engineState::paused && _enginePaused;							}
	bool resumed() const { return _engineState != engineState::paused && _engineState != engineState::resuming;	}

	void process();
	void processFilterReply(		Json::Value json);
	void processRCodeReply(			Json::Value json);
	void processComputeColumnReply(	Json::Value json);
	void processAnalysisReply(		Json::Value json);
	void processEngineResumedReply();
	void processEnginePausedReply();

	void setChannel(IPCChannel * channel)			{ _channel = channel; }
	void setSlaveProcess(QProcess * slaveProcess)	{ _slaveProcess = slaveProcess; }
	int channelNumber()								{ return _channel->channelNumber(); }

	void sendString(std::string str)
	{
#ifdef PRINT_ENGINE_MESSAGES
		std::cout << "sending to jaspEngine: " << str << "\n" << std::endl;
#endif
		_channel->send(str);
	}

private:
	Analysis::Status analysisResultStatusToAnalysStatus(analysisResultStatus result, Analysis * analysis);

	QProcess*	_slaveProcess		= NULL;
	IPCChannel*	_channel			= NULL;
	Analysis*	_analysisInProgress = NULL;
	engineState	_engineState		= engineState::idle;
	int			_ppi				= 96;
	QString		_imageBackground	= "white";
	bool		_enginePaused		= false;

signals:
	void engineTerminated();

	void processFilterErrorMsg(QString error, int requestId);
	void processNewFilterResult(std::vector<bool> filterResult, int requestId);
	void computeColumnErrorTextChanged(QString error);

	void rCodeReturned(QString result, int requestId);

	void computeColumnSucceeded(std::string columnName, std::string warning, bool dataChanged);
	void computeColumnFailed(std::string columnName, std::string error);

public slots:
	void ppiChanged(int newPPI)					{ _ppi = newPPI; }
	void imageBackgroundChanged(QString value)	{ _imageBackground = value; }
};

#endif // ENGINEREPRESENTATION_H
