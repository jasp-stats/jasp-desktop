//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "engine.h"

#include <sstream>
#include <cstdio>

//#include "../JASP-Common/analysisloader.h"
#include <boost/bind.hpp>
#include "../JASP-Common/tempfiles.h"
#include "../JASP-Common/utils.h"
#include "../JASP-Common/sharedmemory.h"
#include <csignal>

#include "rbridge.h"
#include "timers.h"
#include "log.h"

void SendFunctionForJaspresults(const char * msg) { Engine::theEngine()->sendString(msg); }
bool PollMessagesFunctionForJaspResults()
{
	if(Engine::theEngine()->receiveMessages())
	{
		if(Engine::theEngine()->paused())
			return true;
		else
			return Engine::theEngine()->getStatus() == engineAnalysisStatus::changed;
	}
	return false;
}

#ifdef _WIN32

#undef Realloc
#undef Free

#endif

Engine * Engine::_EngineInstance = NULL;

Engine::Engine(int slaveNo, unsigned long parentPID)
	: _slaveNo(slaveNo), _parentPID(parentPID)
{
	JASPTIMER_START(Engine Constructor);
	assert(_EngineInstance == NULL);
	_EngineInstance = this;

	JASPTIMER_START(TempFiles Attach);
	TempFiles::attach(parentPID);
	JASPTIMER_STOP(TempFiles Attach);

	rbridge_setDataSetSource(			boost::bind(&Engine::provideDataSet,				this));
	rbridge_setFileNameSource(			boost::bind(&Engine::provideTempFileName,			this, _1, _2, _3));
	rbridge_setStateFileSource(			boost::bind(&Engine::provideStateFileName,			this, _1, _2));
	rbridge_setJaspResultsFileSource(	boost::bind(&Engine::provideJaspResultsFileName,	this, _1, _2));

	rbridge_setColumnDataAsScaleSource(			boost::bind(&Engine::setColumnDataAsScale,			this, _1, _2));
	rbridge_setColumnDataAsOrdinalSource(		boost::bind(&Engine::setColumnDataAsOrdinal,		this, _1, _2, _3));
	rbridge_setColumnDataAsNominalSource(		boost::bind(&Engine::setColumnDataAsNominal,		this, _1, _2, _3));
	rbridge_setColumnDataAsNominalTextSource(	boost::bind(&Engine::setColumnDataAsNominalText,	this, _1, _2));

	rbridge_setGetDataSetRowCountSource( boost::bind(&Engine::dataSetRowCount, this));

	JASPTIMER_STOP(Engine Constructor);

	JASPTIMER_START(rbridge_init);
	rbridge_init(SendFunctionForJaspresults, PollMessagesFunctionForJaspResults);
	JASPTIMER_STOP(rbridge_init);


}

Engine::~Engine()
{
	TempFiles::deleteAll();

	delete _channel; //shared memory files will be removed in jaspDesktop
	_channel = nullptr;
}

void Engine::run()
{
	JASPTIMER_START(Engine::run startup);
#if defined(QT_DEBUG) || defined(__linux__)
	if (_slaveNo == 0)
	{
		std::string engineInfo = rbridge_check();

		Json::Value v;
		Json::Reader().parse(engineInfo, v);

		Log::log() << v.toStyledString() << std::endl;
	}
#endif

	std::string memoryName = "JASP-IPC-" + std::to_string(_parentPID);
	_channel = new IPCChannel(memoryName, _slaveNo, true);

	sendEngineResumed(); //Then the desktop knows we've finished init.

	while(_engineState != engineState::stopped && ProcessInfo::isParentRunning())
	{
		receiveMessages(100);

		switch(_engineState)
		{
		case engineState::idle:									break;
		case engineState::analysis:			runAnalysis();		break;
		case engineState::paused:			/* Do nothing */
		case engineState::stopped:								break;
		case engineState::resuming:			throw std::runtime_error("Enginestate " + engineStateToString(_engineState) + " should NOT be set as currentState!");
		default:
			Log::log() << "Engine got stuck in engineState " << engineStateToString(_engineState) << " which is not supposed to happen..." << std::endl;
		}

		freeRBridgeColumns();

		if(_engineState != engineState::idle)
			Log::log() << "current Engine state == "<< engineStateToString(_engineState) << std::endl;
	}
}



bool Engine::receiveMessages(int timeout)
{
	std::string data;

	if (_channel->receive(data, timeout))
	{
		Json::Value jsonRequest;
		Json::Reader().parse(data, jsonRequest, false);


		engineState typeRequest = engineStateFromString(jsonRequest.get("typeRequest", Json::nullValue).asString());

#ifdef PRINT_ENGINE_MESSAGES
		Log::log() << "received " << engineStateToString(typeRequest) <<" message" << std::endl << std::flush;
#endif
		switch(typeRequest)
		{
		case engineState::analysis:			receiveAnalysisMessage(jsonRequest);		return true;
		case engineState::filter:			receiveFilterMessage(jsonRequest);			break;
		case engineState::rCode:			receiveRCodeMessage(jsonRequest);			break;
		case engineState::computeColumn:	receiveComputeColumnMessage(jsonRequest);	break;
		case engineState::pauseRequested:	pauseEngine();								break;
		case engineState::resuming:			resumeEngine();								break;
		case engineState::moduleRequest:	receiveModuleRequestMessage(jsonRequest);	break;
		case engineState::stopRequested:	stopEngine();								break;
		case engineState::logCfg:			receiveLogCfg(jsonRequest);					break;
		default:							throw std::runtime_error("Engine::receiveMessages begs you to add your new engineState to it!");
		}
	}

	return false;
}

void Engine::receiveFilterMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		throw std::runtime_error("Unexpected filter message, current state is not idle (" + engineStateToString(_engineState) + ")");

	_engineState				= engineState::filter;
	std::string filter			= jsonRequest.get("filter", "").asString();
	std::string generatedFilter = jsonRequest.get("generatedFilter", "").asString();
	int filterRequestId			= jsonRequest.get("requestId", -1).asInt();

	runFilter(filter, generatedFilter, filterRequestId);
}

void Engine::runFilter(const std::string & filter, const std::string & generatedFilter, int filterRequestId)
{
	try
	{
        std::string strippedFilter		= stringUtils::stripRComments(filter);
		std::vector<bool> filterResult	= rbridge_applyFilter(strippedFilter, generatedFilter);
		std::string RPossibleWarning	= jaspRCPP_getLastErrorMsg();

		sendFilterResult(filterRequestId, filterResult, RPossibleWarning);

	}
	catch(filterException & e)
	{
		sendFilterError(filterRequestId, std::string(e.what()).length() > 0 ? e.what() : "Something went wrong with the filter but it is unclear what.");
	}

	_engineState = engineState::idle;
}

void Engine::sendFilterResult(int filterRequestId, const std::vector<bool> & filterResult, const std::string & warning)
{
	Json::Value filterResponse(Json::objectValue);

	filterResponse["typeRequest"]	= engineStateToString(engineState::filter);
	filterResponse["filterResult"]	= Json::arrayValue;
	filterResponse["requestId"]		= filterRequestId;

	for(bool f : filterResult)	filterResponse["filterResult"].append(f);
	if(warning != "")			filterResponse["filterError"] = warning;

	sendString(filterResponse.toStyledString());
}

void Engine::sendFilterError(int filterRequestId, const std::string & errorMessage)
{
	Json::Value filterResponse = Json::Value(Json::objectValue);

	filterResponse["typeRequest"]	= engineStateToString(engineState::filter);
	filterResponse["filterError"]	= errorMessage;
	filterResponse["requestId"]		= filterRequestId;

	sendString(filterResponse.toStyledString());
}

void Engine::receiveRCodeMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		throw std::runtime_error("Unexpected rCode message, current state is not idle (" + engineStateToString(_engineState) + ")");

	_engineState		= engineState::rCode;
	std::string rCode	= jsonRequest.get("rCode", "").asString();
	int rCodeRequestId	= jsonRequest.get("requestId", -1).asInt();

	runRCode(rCode, rCodeRequestId);
}

// Evaluating arbitrary R code (as string) which returns a string
void Engine::runRCode(const std::string & rCode, int rCodeRequestId)
{
	std::string rCodeResult = jaspRCPP_evalRCode(rCode.c_str());

	if (rCodeResult == "null")	sendRCodeError(rCodeRequestId);
	else						sendRCodeResult(rCodeResult, rCodeRequestId);

	_engineState = engineState::idle;
}


void Engine::sendRCodeResult(const std::string & rCodeResult, int rCodeRequestId)
{
	Json::Value rCodeResponse(Json::objectValue);

	std::string RError				= jaspRCPP_getLastErrorMsg();
	if(RError.size() > 0)
		rCodeResponse["rCodeError"]	= RError;

	rCodeResponse["typeRequest"]	= engineStateToString(engineState::rCode);
	rCodeResponse["rCodeResult"]	= rCodeResult;
	rCodeResponse["requestId"]		= rCodeRequestId;


	sendString(rCodeResponse.toStyledString());
}

void Engine::sendRCodeError(int rCodeRequestId)
{
	Log::log() << "R Code yielded error" << std::endl << std::flush;

	Json::Value rCodeResponse		= Json::objectValue;
	std::string RError				= jaspRCPP_getLastErrorMsg();
	rCodeResponse["typeRequest"]	= engineStateToString(engineState::rCode);
	rCodeResponse["rCodeError"]		= RError.size() == 0 ? "R Code failed for unknown reason. Check that R function returns a string." : RError;
	rCodeResponse["requestId"]		= rCodeRequestId;

	sendString(rCodeResponse.toStyledString());
}

void Engine::receiveComputeColumnMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		throw std::runtime_error("Unexpected compute column message, current state is not idle (" + engineStateToString(_engineState) + ")");

	_engineState = engineState::computeColumn;

	std::string			computeColumnName = jsonRequest.get("columnName", "").asString();
	std::string			computeColumnCode = jsonRequest.get("computeCode", "").asString();
	Column::ColumnType	computeColumnType = Column::columnTypeFromString(jsonRequest.get("columnType", "").asString());

	runComputeColumn(computeColumnName, computeColumnCode, computeColumnType);
}

void Engine::runComputeColumn(const std::string & computeColumnName, const std::string & computeColumnCode, Column::ColumnType computeColumnType)
{
	Log::log() << "Engine::runComputeColumn()" << std::endl;

	static const std::map<Column::ColumnType, std::string> setColumnFunction = {
		{Column::ColumnTypeScale,		".setColumnDataAsScale"},
		{Column::ColumnTypeOrdinal,		".setColumnDataAsOrdinal"},
		{Column::ColumnTypeNominal,		".setColumnDataAsNominal"},
		{Column::ColumnTypeNominalText,	".setColumnDataAsNominalText"}};

	std::string computeColumnCodeComplete	= "local({;calcedVals <- {"+computeColumnCode +"};\n"  "return(toString(" + setColumnFunction.at(computeColumnType) + "('" + computeColumnName +"', calcedVals)));})";
	std::string computeColumnResultStr		= rbridge_evalRCodeWhiteListed(computeColumnCodeComplete);

	Json::Value computeColumnResponse		= Json::objectValue;
	computeColumnResponse["typeRequest"]	= engineStateToString(engineState::computeColumn);
	computeColumnResponse["result"]			= computeColumnResultStr;
	computeColumnResponse["error"]			= jaspRCPP_getLastErrorMsg();
	computeColumnResponse["columnName"]		= computeColumnName;

	sendString(computeColumnResponse.toStyledString());

	_engineState = engineState::idle;
}

void Engine::receiveModuleRequestMessage(const Json::Value & jsonRequest)
{
	_engineState				= engineState::moduleRequest;

	std::string		moduleRequest	= jsonRequest["moduleRequest"].asString();
	std::string		moduleCode		= jsonRequest["moduleCode"].asString();
	std::string		moduleName		= jsonRequest["moduleName"].asString();

	std::string		result			= jaspRCPP_evalRCode(moduleCode.c_str());
	bool			succes			= result == "succes!"; //Defined in DynamicModule::succesResultString()

	Json::Value		jsonAnswer		= Json::objectValue;

	jsonAnswer["moduleRequest"]		= moduleRequest;
	jsonAnswer["moduleName"]		= moduleName;
	jsonAnswer["succes"]			= succes;
	jsonAnswer["error"]				= jaspRCPP_getLastErrorMsg();
	jsonAnswer["typeRequest"]		= engineStateToString(engineState::moduleRequest);

	sendString(jsonAnswer.toStyledString());

	_engineState = engineState::idle;
}

void Engine::receiveAnalysisMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle && _engineState != engineState::analysis)
		throw std::runtime_error("Unexpected analysis message, current state is not idle or analysis (" + engineStateToString(_engineState) + ")");

#ifdef PRINT_ENGINE_MESSAGES
	Log::log() << "Engine::receiveAnalysisMessage:\n" << jsonRequest.toStyledString() << std::endl;
#endif

	int analysisId		= jsonRequest.get("id", -1).asInt();
	performType perform	= performTypeFromString(jsonRequest.get("perform", "run").asString());

	if (analysisId == _analysisId && _analysisStatus == Status::running) // if the current running analysis has changed
		_analysisStatus = (perform == performType::init || (_analysisJaspResults && perform == performType::run)) ? Status::changed : Status::aborted;
	else
	{
		// the new analysis should be init or run (existing analyses will be aborted)
		_analysisId = analysisId;

		switch(perform)
		{
		case performType::init:			_analysisStatus = Status::toInit;		break;
		case performType::run:			_analysisStatus = Status::toRun;		break;
		case performType::saveImg:		_analysisStatus = Status::saveImg;		break;
		case performType::editImg:		_analysisStatus = Status::editImg;		break;
		case performType::rewriteImgs:	_analysisStatus = Status::rewriteImgs;	break;
		default:						_analysisStatus = Status::error;		break;
		}

	}

#ifdef PRINT_ENGINE_MESSAGES
	Log::log() << "msg type was '" << engineAnalysisStatusToString(_analysisStatus) << "'" << std::endl;
#endif

	if(	_analysisStatus == Status::toInit		||
		_analysisStatus == Status::toRun		||
		_analysisStatus == Status::changed		||
		_analysisStatus == Status::saveImg		||
		_analysisStatus == Status::editImg		||
		_analysisStatus == Status::rewriteImgs	  )
	{
		_analysisName			= jsonRequest.get("name",				Json::nullValue).asString();
		_analysisTitle			= jsonRequest.get("title",				Json::nullValue).asString();
		_analysisDataKey		= jsonRequest.get("dataKey",			Json::nullValue).toStyledString();
		_analysisOptions		= jsonRequest.get("options",			Json::nullValue).toStyledString();
		_analysisResultsMeta	= jsonRequest.get("resultsMeta",		Json::nullValue).toStyledString();
		_analysisStateKey		= jsonRequest.get("stateKey",			Json::nullValue).toStyledString();
		_analysisRevision		= jsonRequest.get("revision",			-1).asInt();
		_imageOptions			= jsonRequest.get("image",				Json::nullValue);
		_analysisRFile			= jsonRequest.get("rfile",				"").asString();
		_dynamicModuleCall		= jsonRequest.get("dynamicModuleCall",	"").asString();
		_analysisRequiresInit	= jsonRequest.get("requiresInit",		Json::nullValue).isNull() ? true : jsonRequest.get("requiresInit", true).asBool();
		_ppi					= jsonRequest.get("ppi",				96).asInt();
		_imageBackground		= jsonRequest.get("imageBackground",	"white").asString();

		_analysisJaspResults	= _dynamicModuleCall != "" || jsonRequest.get("jaspResults",	false).asBool();
		_engineState		= engineState::analysis;
	}
}

void Engine::runAnalysis()
{
	Log::log() << "Engine::runAnalysis()" << std::endl;

	if(_analysisStatus == Status::saveImg)		{ saveImage();		return; }
	if(_analysisStatus == Status::editImg)		{ editImage();		return; }
	if(_analysisStatus == Status::rewriteImgs)	{ rewriteImages();	return; }

	if(_analysisStatus == Status::empty || _analysisStatus == Status::aborted)
	{
		_analysisStatus	= Status::empty;
		_engineState	= engineState::idle;
		return;
	}

	if(_analysisStatus == Status::toInit && !_analysisJaspResults)	_analysisStatus = Status::initing;
	else															_analysisStatus = Status::running;

	std::string perform					= _analysisStatus == Status::initing ? "init" : "run";

	RCallback callback					= boost::bind(&Engine::callback, this, _1, _2);

	_currentAnalysisKnowsAboutChange	= false;

	_analysisResultsString = _dynamicModuleCall != "" ?
			rbridge_runModuleCall(_analysisName, _analysisTitle, _dynamicModuleCall, _analysisDataKey, _analysisOptions, _analysisStateKey, perform, _ppi, _analysisId, _analysisRevision, _imageBackground)
		:	rbridge_run(_analysisName, _analysisTitle, _analysisRFile, _analysisRequiresInit, _analysisDataKey, _analysisOptions, _analysisResultsMeta, _analysisStateKey, _analysisId, _analysisRevision, perform, _ppi, _imageBackground, callback, _analysisJaspResults);

	if (_analysisStatus == Status::initing || _analysisStatus == Status::running)  // if status hasn't changed
		receiveMessages();

	if (_analysisStatus == Status::toInit || _analysisStatus == Status::aborted || _analysisStatus == Status::error || _analysisStatus == Status::exception)
	{
		// analysis was aborted, and we shouldn't send the results
		return;
	}
	else if (_analysisStatus == Status::changed && (_currentAnalysisKnowsAboutChange == false || _analysisResultsString == "null"))
	{
		// analysis was changed, and the analysis either did not know about the change (because it did not call a callback),
		// or it could not incorporate the changes (returned null). In both cases it needs to be re-run, and results should not be sent

		_analysisStatus = Status::toInit;

		if (_analysisResultsString == "null")
			TempFiles::deleteList(TempFiles::retrieveList(_analysisId));
		return;
	}
	else
	{
		Json::Reader().parse(_analysisResultsString, _analysisResults, false);

		if(!_analysisJaspResults)
		{
			_analysisStatus		= _analysisStatus == Status::initing ? Status::inited : Status::complete;
			_progress	= -1;
			sendAnalysisResults();
		}

		_engineState	= engineState::idle;
		_analysisStatus	= Status::empty;

		removeNonKeepFiles(_analysisResults.isObject() ? _analysisResults.get("keep", Json::nullValue) : Json::nullValue);

	}
}

void Engine::saveImage()
{
	std::string name	= _imageOptions.get("name", Json::nullValue).asString();
	std::string type	= _imageOptions.get("type", Json::nullValue).asString();
	int height			= _imageOptions.get("height", Json::nullValue).asInt();
	int width			= _imageOptions.get("width", Json::nullValue).asInt();

	std::string result = jaspRCPP_saveImage(name.c_str(), type.c_str(), height, width, _ppi, _imageBackground.c_str());

	Json::Reader().parse(result, _analysisResults, false);

	_analysisStatus								= Status::complete;
	_analysisResults["results"]["inputOptions"]	= _imageOptions;
	_progress									= -1;
	sendAnalysisResults();

	_analysisStatus								= Status::empty;
	_engineState								= engineState::idle;
}

void Engine::editImage()
{
	std::string name	= _imageOptions.get("name", Json::nullValue).asString();
	std::string type	= _imageOptions.get("type", Json::nullValue).asString();
	int height			= _imageOptions.get("height", Json::nullValue).asInt();
	int width			= _imageOptions.get("width", Json::nullValue).asInt();
	std::string result	= jaspRCPP_editImage(name.c_str(), type.c_str(), height, width, _ppi, _imageBackground.c_str());

	Json::Reader().parse(result, _analysisResults, false);

	_analysisStatus			= Status::complete;
	_progress				= -1;
	sendAnalysisResults();

	_analysisStatus			= Status::empty;
	_engineState			= engineState::idle;
}

void Engine::rewriteImages()
{
	jaspRCPP_rewriteImages(_ppi, _imageBackground.c_str());

	_analysisStatus				= Status::complete;
	_analysisResults			= Json::Value();
	_analysisResults["status"]	= analysisResultStatusToString(analysisResultStatus::imagesRewritten);
	_progress					= -1;
	sendAnalysisResults();

	_analysisStatus				= Status::empty;
	_engineState				= engineState::idle;
}


analysisResultStatus Engine::getStatusToAnalysisStatus()
{
	switch (_analysisStatus)
	{
	case Status::inited:	return analysisResultStatus::inited;
	case Status::running:
	case Status::changed:	return analysisResultStatus::running;
	case Status::complete:	return analysisResultStatus::complete;
	default:				return analysisResultStatus::error;
	}
}

void Engine::sendAnalysisResults()
{
	Json::Value response = Json::Value(Json::objectValue);

	response["typeRequest"]	= engineStateToString(engineState::analysis);
	response["id"]			= _analysisId;
	response["name"]		= _analysisName;
	response["revision"]	= _analysisRevision;
	response["progress"]	= _progress;

	bool					sensibleResultsStatus	= _analysisResults.isObject() && _analysisResults.get("status", Json::nullValue) != Json::nullValue;
	analysisResultStatus	resultStatus			= !sensibleResultsStatus ? getStatusToAnalysisStatus() : analysisResultStatusFromString(_analysisResults["status"].asString());

	response["results"] = _analysisResults.get("results", _analysisResults);
	response["status"]  = analysisResultStatusToString(resultStatus);

	sendString(response.toStyledString());
}

void Engine::removeNonKeepFiles(const Json::Value & filesToKeepValue)
{
	std::vector<std::string> filesToKeep;

	if (filesToKeepValue.isArray())
	{
		for (size_t i = 0; i < filesToKeepValue.size(); i++)
		{
			Json::Value fileToKeepValue = filesToKeepValue.get(i, Json::nullValue);
			if ( ! fileToKeepValue.isString())
				continue;

			filesToKeep.push_back(fileToKeepValue.asString());
		}
	}
	else if (filesToKeepValue.isString())
	{
		filesToKeep.push_back(filesToKeepValue.asString());
	}

	std::vector<std::string> tempFilesFromLastTime = TempFiles::retrieveList(_analysisId);

	Utils::remove(tempFilesFromLastTime, filesToKeep);

	TempFiles::deleteList(tempFilesFromLastTime);
}

DataSet * Engine::provideDataSet()
{
	return SharedMemory::retrieveDataSet(_parentPID);
}

void Engine::provideStateFileName(std::string &root, std::string &relativePath)
{
	return TempFiles::createSpecific("state", _analysisId, root, relativePath);
}

void Engine::provideJaspResultsFileName(std::string &root, std::string &relativePath)
{
	return TempFiles::createSpecific("jaspResults.json", _analysisId, root, relativePath);
}

void Engine::provideTempFileName(const std::string &extension, std::string &root, std::string &relativePath)
{
	TempFiles::create(extension, _analysisId, root, relativePath);
}


std::string Engine::callback(const std::string &results, int progress)
{
	receiveMessages();

	if (_analysisStatus == Status::aborted || _analysisStatus == Status::toInit || _analysisStatus == Status::toRun)
		return "{ \"status\" : \"aborted\" }"; // abort

	if (_analysisStatus == Status::changed && _currentAnalysisKnowsAboutChange)
	{
		_analysisStatus = Status::running;
		_currentAnalysisKnowsAboutChange = false;
	}

	if (results != "null")
	{
		_analysisResultsString = results;

		Json::Reader().parse(_analysisResultsString, _analysisResults, false);

		_progress = progress;

		sendAnalysisResults();
	}
	else if (progress >= 0 && _analysisStatus == Status::running)
	{
		_analysisResultsString	= "";
		_analysisResults		= Json::nullValue;
		_progress				= progress;

		sendAnalysisResults();

	}

	if (_analysisStatus == Status::changed)
	{
		_currentAnalysisKnowsAboutChange = true; // because we're telling it now
		return "{ \"status\" : \"changed\", \"options\" : " + _analysisOptions + " }";
	}
	else if (_analysisStatus == Status::aborted)	return "{ \"status\" : \"aborted\" }";

	return "{ \"status\" : \"ok\" }";
}

bool Engine::isColumnNameOk(std::string columnName)
{
	if(columnName == "")
		return false;

	try
	{
		provideDataSet()->columns().findIndexByName(columnName);
		return true;
	}
	catch(columnNotFound &)
	{
		return false;
	}
}

bool Engine::setColumnDataAsNominalOrOrdinal(bool isOrdinal, const std::string & columnName, std::vector<int> & data, const std::map<int, std::string> & levels)
{
	std::map<int, int> uniqueInts;

	for(auto keyval : levels)
	{
		size_t convertedChars;

		try {
			int asInt = std::stoi(keyval.second, &convertedChars);

			if(convertedChars == keyval.second.size()) //It was a number!
				uniqueInts[keyval.first] = asInt;
		}
		catch(std::invalid_argument e) {}
	}

	if(uniqueInts.size() == levels.size()) //everything was an int!
	{
		for(auto & dat : data)
			if(dat != INT_MIN)
				dat = uniqueInts[dat];

		if(isOrdinal)	return	provideDataSet()->columns()[columnName].overwriteDataWithOrdinal(data);
		else			return	provideDataSet()->columns()[columnName].overwriteDataWithNominal(data);
	}
	else
	{
		if(isOrdinal)	return	provideDataSet()->columns()[columnName].overwriteDataWithOrdinal(data, levels);
		else			return	provideDataSet()->columns()[columnName].overwriteDataWithNominal(data, levels);
	}
}

void Engine::stopEngine()
{
	Log::log() << "Engine::stopEngine() received, closing engine." << std::endl;

	switch(_engineState)
	{
	default:							/* everything not mentioned is fine */	break;
	case engineState::analysis:			_analysisStatus = Status::aborted;		break;
	case engineState::filter:
	case engineState::computeColumn:	throw std::runtime_error("Unexpected data synch during " + engineStateToString(_engineState) + " somehow, you should not expect to see this exception ever.");
	};

	_engineState = engineState::stopped;

	freeRBridgeColumns();
	SharedMemory::unloadDataSet();
	sendEngineStopped();
}

void Engine::sendEngineStopped()
{
	Json::Value rCodeResponse		= Json::objectValue;
	rCodeResponse["typeRequest"]	= engineStateToString(_engineState);
	sendString(rCodeResponse.toStyledString());
}

void Engine::pauseEngine()
{
	switch(_engineState)
	{
	default:							/* everything not mentioned is fine */	break;
	case engineState::analysis:			_analysisStatus = Status::aborted;		break;
	case engineState::filter:
	case engineState::computeColumn:	throw std::runtime_error("Unexpected data synch during " + engineStateToString(_engineState) + " somehow, you should not expect to see this exception ever.");
	};

	_engineState = engineState::paused;

	freeRBridgeColumns();
	SharedMemory::unloadDataSet();
	sendEnginePaused();
}

void Engine::sendEnginePaused()
{
	Log::log() << "Engine paused" << std::endl;

	Json::Value rCodeResponse		= Json::objectValue;
	rCodeResponse["typeRequest"]	= engineStateToString(engineState::paused);

	sendString(rCodeResponse.toStyledString());
}

void Engine::resumeEngine()
{
	_engineState = engineState::idle;
	sendEngineResumed();
}

void Engine::sendEngineResumed()
{
	Log::log() << "Engine resuming" << std::endl;

	Json::Value rCodeResponse		= Json::objectValue;
	rCodeResponse["typeRequest"]	= engineStateToString(engineState::resuming);

	sendString(rCodeResponse.toStyledString());
}

void Engine::receiveLogCfg(const Json::Value & jsonRequest)
{
	Log::log() << "Log Config received" << std::endl;

	Log::parseLogCfgMsg(jsonRequest);

	Json::Value logCfgResponse		= Json::objectValue;
	logCfgResponse["typeRequest"]	= engineStateToString(engineState::logCfg);

	sendString(logCfgResponse.toStyledString());

	_engineState = engineState::idle;
}
