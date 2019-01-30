#include "enginerepresentation.h"
#include "utilities/settings.h"

EngineRepresentation::EngineRepresentation(IPCChannel * channel, QProcess * slaveProcess, QObject * parent)
	: QObject(parent), _slaveProcess(slaveProcess), _channel(channel)
{
	_imageBackground = Settings::value(Settings::IMAGE_BACKGROUND).toString();
}

EngineRepresentation::~EngineRepresentation()
{
	if(_slaveProcess != NULL)
	{

		_slaveProcess->terminate();
		_slaveProcess->kill();
	}
}

void EngineRepresentation::clearAnalysisInProgress()
{
	_analysisInProgress = NULL;
	_engineState		= engineState::idle;
}

void EngineRepresentation::setAnalysisInProgress(Analysis* analysis)
{
	if(_engineState == engineState::analysis)
	{
		if(_analysisInProgress == analysis)	return; //we are already busy with this analysis so everything is fine
		else								throw std::runtime_error("Engine " + std::to_string(_channel->channelNumber()) + " is running another analysis. Yet you are trying to set an analysis on it..");
	}

	if(_engineState != engineState::idle)	throw std::runtime_error("Engine " + std::to_string(_channel->channelNumber()) + " is not idle! Yet you are trying to set an analysis on it..");

	_analysisInProgress = analysis;
	_engineState		= engineState::analysis;
}

void EngineRepresentation::process()
{
	if (_engineState == engineState::idle)
	{
		if(_pauseRequested)	sendPauseEngine();
		return;
	}

	std::string data;

	if (_channel->receive(data))
	{
#ifdef PRINT_ENGINE_MESSAGES
		std::cout << "message received" <<std::endl;
#endif

		Json::Value json;
		Json::Reader().parse(data, json);

		if(!json.get("typeRequest", Json::nullValue).isString() && _engineState != engineState::analysis)
			throw std::runtime_error("Malformed reply from engine!");

		engineState typeRequest = engineStateFromString(json.get("typeRequest", "analysis").asString());

		switch(typeRequest)
		{
		case engineState::filter:			processFilterReply(json);			break;
		case engineState::rCode:			processRCodeReply(json);			break;
		case engineState::analysis:			processAnalysisReply(json);			break;
		case engineState::computeColumn:	processComputeColumnReply(json);	break;
		case engineState::paused:			processEnginePausedReply();			break;
		case engineState::resuming:			processEngineResumedReply();		break;
		case engineState::moduleRequest:	processModuleRequestReply(json);	break;
		default:							throw std::logic_error("If you define new engineStates you should add them to the switch in EngineRepresentation::process()!");
		}
	}
}


void EngineRepresentation::runScriptOnProcess(RFilterStore * filterStore)
{
	Json::Value json = Json::Value(Json::objectValue);

	_engineState			= engineState::filter;
	json["typeRequest"]		= engineStateToString(_engineState);
	json["generatedFilter"] = filterStore->generatedfilter.toStdString();
	json["requestId"]		= filterStore->requestId;


	QString dataFilter = filterStore->script == "" ? "*" : filterStore->script;
	json["filter"] = dataFilter.toStdString();

#ifdef JASP_DEBUG
	std::cout << "sending filter with requestID " << filterStore->requestId << " to engine" << std::endl;
#endif

	sendString(json.toStyledString());
}

void EngineRepresentation::processFilterReply(Json::Value json)
{
	if(_engineState != engineState::filter)
		throw std::runtime_error("Received an unexpected filter reply!");
	_engineState = engineState::idle;

#ifdef PRINT_ENGINE_MESSAGES
			std::cout << "msg is filter reply" << std::endl << std::flush;
#endif

	int requestId = json.get("requestId", -1).asInt();

	if(json.get("filterResult", Json::Value(Json::intValue)).isArray()) //If the result is an array then it came from the engine.
	{
		std::vector<bool> filterResult;
		for(Json::Value & jsonResult : json.get("filterResult", Json::Value(Json::arrayValue)))
			filterResult.push_back(jsonResult.asBool());

		emit processNewFilterResult(filterResult, requestId);

		if(json.get("filterError", "").asString() != "")
			emit processFilterErrorMsg(QString::fromStdString(json.get("filterError", "there was a warning").asString()), requestId);
	}
	else
		emit processFilterErrorMsg(QString::fromStdString(json.get("filterError", "something went wrong").asString()), requestId);
}

void EngineRepresentation::runScriptOnProcess(RScriptStore * scriptStore)
{
	Json::Value json = Json::Value(Json::objectValue);

	_engineState			= engineState::rCode;
	json["typeRequest"]		= engineStateToString(_engineState);
	json["rCode"]			= scriptStore->script.toStdString();
	json["requestId"]		= scriptStore->requestId;

	sendString(json.toStyledString());
}


void EngineRepresentation::processRCodeReply(Json::Value json)
{
	if(_engineState != engineState::rCode)
		throw std::runtime_error("Received an unexpected rCode reply!");
	_engineState = engineState::idle;

	std::string rCodeResult = json.get("rCodeResult", "").asString();
	int requestId			= json.get("requestId", -1).asInt();

	emit rCodeReturned(QString::fromStdString(rCodeResult), requestId);

#ifdef JASP_DEBUG
		std::cout << "rCode reply for request (" << requestId << ") returned: " << rCodeResult << " with error: '" << json.get("rCodeError", "no error") << "'\n" <<  std::flush;
#endif

}


void EngineRepresentation::runScriptOnProcess(RComputeColumnStore * computeColumnStore)
{
	Json::Value json = Json::Value(Json::objectValue);

	_engineState			= engineState::computeColumn;

	json["typeRequest"]		= engineStateToString(_engineState);
	json["columnName"]		= computeColumnStore->columnName.toStdString();
	json["computeCode"]		= computeColumnStore->script.toStdString();
	json["columnType"]		= Column::columnTypeToString(computeColumnStore->columnType);

	sendString(json.toStyledString());
}


void EngineRepresentation::processComputeColumnReply(Json::Value json)
{
	if(_engineState != engineState::computeColumn)
		throw std::runtime_error("Received an unexpected computeColumn reply!");
	_engineState = engineState::idle;


	std::string result		= json.get("result", "some string that is not 'TRUE' or 'FALSE'").asString();
	std::string error		= json.get("error", "").asString();
	std::string columnName	= json.get("columnName", "").asString();

	if(result == "TRUE")		emit computeColumnSucceeded(columnName, error, true);
	else if(result == "FALSE")	emit computeColumnSucceeded(columnName, error, false);
	else						emit computeColumnFailed(columnName, error == "" ? "Unknown Error" : error);
}

void EngineRepresentation::runAnalysisOnProcess(Analysis *analysis)
{
#ifdef PRINT_ENGINE_MESSAGES
	std::cout << "send request for analysis-id #" << analysis->id() << " to jaspEngine on channel #" << channelNumber() << std::endl;
#endif

	setAnalysisInProgress(analysis);

	Json::Value json(analysis->createAnalysisRequestJson(_ppi, _imageBackground.toStdString()));
	_channel->send(json.toStyledString());

#ifdef PRINT_ENGINE_MESSAGES
	std::cout << "sending: " << json.toStyledString() << std::endl;
#endif

	if(analysis->isAborted())
		clearAnalysisInProgress();

}

Analysis::Status EngineRepresentation::analysisResultStatusToAnalysStatus(analysisResultStatus result, Analysis * analysis)
{
	switch(result)
	{
	case analysisResultStatus::error:			return Analysis::Error;
	case analysisResultStatus::exception:		return Analysis::Exception;
	case analysisResultStatus::imageSaved:
	case analysisResultStatus::imageEdited:
	case analysisResultStatus::imagesRewritten:
	case analysisResultStatus::complete:		return Analysis::Complete;
	case analysisResultStatus::inited:			return Analysis::Inited;
	case analysisResultStatus::running:			return Analysis::Running;
	default:									throw std::logic_error("When you define new analysisResultStatuses you should add them to EngineRepresentation::analysisResultStatusToAnalysStatus!");
	}
}

void EngineRepresentation::analysisRemoved(Analysis * analysis)
{
	if(_engineState != engineState::analysis || _analysisInProgress != analysis)
		return;

	runAnalysisOnProcess(analysis); //should abort
	clearAnalysisInProgress();
}

void EngineRepresentation::processAnalysisReply(Json::Value json)
{
	if(_engineState == engineState::paused || _engineState == engineState::resuming || _engineState == engineState::idle)
		return;

	if(_engineState != engineState::analysis)
		return;

#ifdef PRINT_ENGINE_MESSAGES
	std::cout << "Analysis reply: " << json.toStyledString() << std::endl;
#endif

	int id						= json.get("id",		-1).asInt();
	int revision				= json.get("revision",	-1).asInt();
	int progress				= json.get("progress",	-1).asInt();
	Json::Value results			= json.get("results",	Json::nullValue);

	analysisResultStatus status	= analysisResultStatusFromString(json.get("status", "error").asString());
	Analysis *analysis			= _analysisInProgress;

	if (analysis->id() != id || analysis->revision() < revision)
		throw std::runtime_error("Received results for wrong analysis!");

	if(analysis->revision() > revision) //I guess we changed some option or something?
		return;

	analysis->setStatus(analysisResultStatusToAnalysStatus(status, analysis));

	switch(status)
	{
	case analysisResultStatus::imageSaved:
		analysis->imageSaved(results);
		clearAnalysisInProgress();
		break;


	case analysisResultStatus::imageEdited:
		analysis->imageEdited(results);
		clearAnalysisInProgress();
		break;

	case analysisResultStatus::imagesRewritten:
		analysis->imagesRewritten();
		clearAnalysisInProgress();
		break;

	case analysisResultStatus::error:
		analysis->setResults(results);
		clearAnalysisInProgress();

		for(std::string col : analysis->columnsCreated())
			emit computeColumnFailed(col, "Analysis had an error..");
		break;

	case analysisResultStatus::exception:
	case analysisResultStatus::inited:
	case analysisResultStatus::complete:
		analysis->setResults(results);
		clearAnalysisInProgress();

		//createdColumns and if it succeeded or not should actually be communicated through jaspColumn or something, to be created
		for(std::string col : analysis->columnsCreated())
			emit computeColumnSucceeded(col, "", true);
		break;

	case analysisResultStatus::running:
	default:
		analysis->setResults(results, progress);
		break;
	}
}

void EngineRepresentation::handleRunningAnalysisStatusChanges()
{
	if (_engineState != engineState::analysis)
		return;

	if(_analysisInProgress->isEmpty() || _analysisInProgress->isAborted())
		runAnalysisOnProcess(_analysisInProgress);
}

void EngineRepresentation::pauseEngine()
{
	_pauseRequested = true;
}

void EngineRepresentation::sendPauseEngine()
{
	Json::Value json		= Json::Value(Json::objectValue);
	_engineState			= engineState::pauseRequested;
	json["typeRequest"]		= engineStateToString(_engineState);

#ifdef JASP_DEBUG
	std::cout << "informing engine that it ought to pause for a bit" << std::endl;
#endif

	sendString(json.toStyledString());
}

void EngineRepresentation::resumeEngine()
{
	if(_engineState != engineState::paused)
		throw std::runtime_error("Attempt to resume engine made but it isn't paused");

	_pauseRequested			= false;
	_engineState			= engineState::resuming;
	Json::Value json		= Json::Value(Json::objectValue);
	json["typeRequest"]		= engineStateToString(_engineState);

#ifdef JASP_DEBUG
	std::cout << "informing engine that it may resume" << std::endl;
#endif

	sendString(json.toStyledString());
}

void EngineRepresentation::processEnginePausedReply()
{
	if(_engineState != engineState::pauseRequested)
		throw std::runtime_error("Received an unexpected engine paused reply!");

	_engineState = engineState::paused;
}

void EngineRepresentation::processEngineResumedReply()
{
	if(_engineState != engineState::resuming)
		throw std::runtime_error("Received an unexpected engine resumed reply!");

	_engineState = engineState::idle;
}

void EngineRepresentation::runModuleRequestOnProcess(Json::Value request)
{
	_engineState			= engineState::moduleRequest;
	request["typeRequest"]	= engineStateToString(_engineState);

	sendString(request.toStyledString());
}

void EngineRepresentation::processModuleRequestReply(Json::Value json)
{
	if(_engineState != engineState::moduleRequest)
		throw std::runtime_error("Received an unexpected moduleRequest reply!");
	_engineState = engineState::idle;

	moduleStatus moduleRequest	= moduleStatusFromString(json["moduleRequest"].asString());
	bool succes					= json["succes"].asBool();
	std::string moduleName		= json["moduleName"].asString();
	auto getError				= [&](){ return json.get("error", "Unknown error").asString(); };

	switch(moduleRequest)
	{
	case moduleStatus::installNeeded:
		if(succes)	emit moduleInstallationSucceeded(moduleName);
		else		emit moduleInstallationFailed(moduleName, getError());
		break;

	case moduleStatus::loadingNeeded:
		if(succes)	emit moduleLoadingSucceeded(moduleName, channelNumber());
		else		emit moduleLoadingFailed(moduleName, getError(), channelNumber());
		break;

	case moduleStatus::unloadingNeeded:
		emit moduleUnloadingFinished(moduleName, channelNumber());
		break;

	default:
		throw std::runtime_error("Unsupported module request reply to EngineRepresentation::processModuleRequestReply!");
	}
}

void EngineRepresentation::ppiChanged(int newPPI)
{
	_ppi = newPPI;
	std::cout << "ppi for engineRep set to: " << _ppi << std::endl;

	rerunRunningAnalysis();
}

void EngineRepresentation::imageBackgroundChanged(QString value)
{
	_imageBackground = value;

	rerunRunningAnalysis();
}

void  EngineRepresentation::rerunRunningAnalysis()
{
	if(_engineState == engineState::analysis && _analysisInProgress != nullptr)
		_analysisInProgress->refresh();
}
