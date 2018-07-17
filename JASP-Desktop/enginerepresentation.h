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
#include "modules/dynamicmodules.h"

class EngineRepresentation : public QObject
{
	Q_OBJECT

public:
	EngineRepresentation(IPCChannel * channel, QProcess * slaveProcess, QObject * parent = NULL) : QObject(parent), _slaveProcess(slaveProcess), _channel(channel) {}
	~EngineRepresentation();
	void clearAnalysisInProgress();
	void setAnalysisInProgress(Analysis* analysis);

	bool isIdle() { return _engineState == engineState::idle; }

	void handleRunningAnalysisStatusChanges();

	void runScriptOnProcess(RFilterStore * filterStore);
	void runScriptOnProcess(RScriptStore * scriptStore);
	void runScriptOnProcess(RComputeColumnStore * computeColumnStore);
	void runAnalysisOnProcess(Analysis *analysis);
	void runModuleRequestOnProcess(Json::Value request);

	void process();
	void processRCodeReply(			Json::Value json);
	void processFilterReply(		Json::Value json);
	void processAnalysisReply(		Json::Value json);
	void processComputeColumnReply(	Json::Value json);
	void processModuleRequestReply(	Json::Value json);

	void setChannel(IPCChannel * channel)			{ _channel = channel; }
	void setSlaveProcess(QProcess * slaveProcess)	{ _slaveProcess = slaveProcess; }
	int channelNumber()								{ return _channel->channelNumber(); }

	void sendString(std::string str)				{ _channel->send(str); }

private:
	Analysis::Status analysisResultStatusToAnalysStatus(analysisResultStatus result, Analysis * analysis);

	QProcess*	_slaveProcess		= NULL;
	IPCChannel*	_channel			= NULL;
	Analysis*	_analysisInProgress = NULL;
	engineState	_engineState		= engineState::idle;
	int			_ppi				= 96;

signals:
	void engineTerminated();

	void filterUpdated();
	void filterErrorTextChanged(	QString error);
	void dataFilterChanged(			QString newDataFilter);
	void processNewFilterResult(	std::vector<bool> filterResult);

	void rCodeReturned(QString result, int requestId);

	void computeColumnErrorTextChanged(	QString error);
	void computeColumnSucceeded(		std::string columnName, std::string warning);
	void computeColumnFailed(			std::string columnName, std::string error);

	void moduleInstallationSucceeded(	std::string moduleName);
	void moduleInstallationFailed(		std::string moduleName, std::string errorMessage);
	void moduleLoadingSucceeded(		std::string moduleName);
	void moduleLoadingFailed(			std::string moduleName, std::string errorMessage);

public slots:
	void ppiChanged(int newPPI) { _ppi = newPPI; }
};

#endif // ENGINEREPRESENTATION_H
