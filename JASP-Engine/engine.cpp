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

void SendFunctionForJaspresults(const char * msg) { Engine::theEngine()->sendString(msg); }
bool PollMessagesFunctionForJaspResults()
{
	if(Engine::theEngine()->receiveMessages())
	{
		if(Engine::theEngine()->paused())
			return true;
		else
			return Engine::theEngine()->getStatus() == Engine::changed;
	}
	return false;
}

#ifdef __WIN32__

#undef Realloc
#undef Free

#endif

Engine * Engine::_EngineInstance = NULL;

Engine::Engine(int slaveNo, unsigned long parentPID) : _slaveNo(slaveNo), _parentPID(parentPID)
{
	assert(_EngineInstance == NULL);
	_EngineInstance = this;

	TempFiles::attach(parentPID);

	rbridge_setDataSetSource(			boost::bind(&Engine::provideDataSet,				this));
	rbridge_setFileNameSource(			boost::bind(&Engine::provideTempFileName,			this, _1, _2, _3));
	rbridge_setStateFileSource(			boost::bind(&Engine::provideStateFileName,			this, _1, _2));
	rbridge_setJaspResultsFileSource(	boost::bind(&Engine::provideJaspResultsFileName,	this, _1, _2));

	rbridge_setColumnDataAsScaleSource(			boost::bind(&Engine::setColumnDataAsScale,			this, _1, _2));
	rbridge_setColumnDataAsOrdinalSource(		boost::bind(&Engine::setColumnDataAsOrdinal,		this, _1, _2, _3));
	rbridge_setColumnDataAsNominalSource(		boost::bind(&Engine::setColumnDataAsNominal,		this, _1, _2, _3));
	rbridge_setColumnDataAsNominalTextSource(	boost::bind(&Engine::setColumnDataAsNominalText,	this, _1, _2));

	rbridge_setGetDataSetRowCountSource( boost::bind(&Engine::dataSetRowCount, this));

	//usleep(10000000);

	rbridge_init(SendFunctionForJaspresults, PollMessagesFunctionForJaspResults);
}

void Engine::run()
{
#if defined(QT_DEBUG) || defined(__linux__)
	if (_slaveNo == 0)
	{
		std::string engineInfo = rbridge_check();

		Json::Value v;
		Json::Reader().parse(engineInfo, v);

		std::cout << v.toStyledString() << "\n";
		std::cout.flush();
	}
#endif

	std::string memoryName = "JASP-IPC-" + std::to_string(_parentPID);
	_channel = new IPCChannel(memoryName, _slaveNo, true);

	while(ProcessInfo::isParentRunning())
	{
		receiveMessages(100);

		switch(_currentEngineState)
		{
		case engineState::idle:									break;
		case engineState::analysis:			runAnalysis();		break;
		case engineState::paused:			/* Do nothing */	break;
		case engineState::resuming:			throw std::runtime_error("Enginestate " + engineStateToString(_currentEngineState) + " should NOT be set as currentState!");
		default:
			std::cout << "Engine got stuck in engineState " << engineStateToString(_currentEngineState) << " which is not supposed to happen..." << std::endl;
		}

		freeRBridgeColumns();
	}

	boost::interprocess::shared_memory_object::remove(memoryName.c_str());
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
		std::cout << "received " << engineStateToString(typeRequest) <<" message" << std::endl << std::flush;
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
		default:							throw std::runtime_error("Engine::receiveMessages begs you to add your new engineState to it!");
		}
	}

	return false;
}

void Engine::receiveFilterMessage(Json::Value jsonRequest)
{
	if(_currentEngineState != engineState::idle)
		throw std::runtime_error("Unexpected filter message, current state is not idle (" + engineStateToString(_currentEngineState) + ")");

	_currentEngineState			= engineState::filter;
	std::string filter			= jsonRequest.get("filter", "").asString();
	std::string generatedFilter = jsonRequest.get("generatedFilter", "").asString();
	int filterRequestId			= jsonRequest.get("requestId", -1).asInt();

	runFilter(filter, generatedFilter, filterRequestId);
}

void Engine::runFilter(std::string filter, std::string generatedFilter, int filterRequestId)
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

	_currentEngineState = engineState::idle;
}

void Engine::sendFilterResult(int filterRequestId, std::vector<bool> filterResult, std::string warning)
{
	Json::Value filterResponse(Json::objectValue);

	filterResponse["typeRequest"]	= engineStateToString(engineState::filter);
	filterResponse["filterResult"]	= Json::arrayValue;
	filterResponse["requestId"]		= filterRequestId;

	for(bool f : filterResult)	filterResponse["filterResult"].append(f);
	if(warning != "")			filterResponse["filterError"] = warning;

	sendString(filterResponse.toStyledString());
}

void Engine::sendFilterError(int filterRequestId, std::string errorMessage)
{
	Json::Value filterResponse = Json::Value(Json::objectValue);

	filterResponse["typeRequest"]	= engineStateToString(engineState::filter);
	filterResponse["filterError"]	= errorMessage;
	filterResponse["requestId"]		= filterRequestId;

	sendString(filterResponse.toStyledString());
}

void Engine::receiveRCodeMessage(Json::Value jsonRequest)
{
	if(_currentEngineState != engineState::idle)
		throw std::runtime_error("Unexpected rCode message, current state is not idle (" + engineStateToString(_currentEngineState) + ")");

	_currentEngineState	= engineState::rCode;
	std::string rCode	= jsonRequest.get("rCode", "").asString();
	int rCodeRequestId	= jsonRequest.get("requestId", -1).asInt();

	runRCode(rCode, rCodeRequestId);
}

// Evaluating arbitrary R code (as string) which returns a string
void Engine::runRCode(std::string rCode, int rCodeRequestId)
{
	std::string rCodeResult = jaspRCPP_evalRCode(rCode.c_str());

	if (rCodeResult == "null")	sendRCodeError(rCodeRequestId);
	else						sendRCodeResult(rCodeResult, rCodeRequestId);

	_currentEngineState = engineState::idle;
}


void Engine::sendRCodeResult(std::string rCodeResult, int rCodeRequestId)
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
	std::cout << "R Code yielded error" << std::endl << std::flush;

	Json::Value rCodeResponse		= Json::objectValue;
	std::string RError				= jaspRCPP_getLastErrorMsg();
	rCodeResponse["typeRequest"]	= engineStateToString(engineState::rCode);
	rCodeResponse["rCodeError"]		= RError.size() == 0 ? "R Code failed for unknown reason. Check that R function returns a string." : RError;
	rCodeResponse["requestId"]		= rCodeRequestId;

	sendString(rCodeResponse.toStyledString());
}

void Engine::receiveComputeColumnMessage(Json::Value jsonRequest)
{
	if(_currentEngineState != engineState::idle)
		throw std::runtime_error("Unexpected compute column message, current state is not idle (" + engineStateToString(_currentEngineState) + ")");

	_currentEngineState = engineState::computeColumn;

	std::string			computeColumnName = jsonRequest.get("columnName", "").asString();
	std::string			computeColumnCode = jsonRequest.get("computeCode", "").asString();
	Column::ColumnType	computeColumnType = Column::columnTypeFromString(jsonRequest.get("columnType", "").asString());

	runComputeColumn(computeColumnName, computeColumnCode, computeColumnType);
}

void Engine::runComputeColumn(std::string computeColumnName, std::string computeColumnCode, Column::ColumnType computeColumnType)
{
#ifdef JASP_DEBUG
	std::cout << "Engine::runComputeColumn()" << std::endl;
#endif

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

	_currentEngineState = engineState::idle;
}

void Engine::receiveModuleRequestMessage(Json::Value jsonRequest)
{
	_currentEngineState = engineState::moduleRequest;

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

	_currentEngineState = engineState::idle;
}

void Engine::receiveAnalysisMessage(Json::Value jsonRequest)
{
	if(_currentEngineState != engineState::idle && _currentEngineState != engineState::analysis)
		throw std::runtime_error("Unexpected compute column message, current state is not idle or analysis (" + engineStateToString(_currentEngineState) + ")");

#ifdef PRINT_ENGINE_MESSAGES
	std::cout << jsonRequest.toStyledString() << std::endl;
#endif

	int analysisId		= jsonRequest.get("id", -1).asInt();
	performType perform	= performTypeFromString(jsonRequest.get("perform", "run").asString());

	if (analysisId == _analysisId && _analysisStatus == running) // if the current running analysis has changed
		_analysisStatus = (perform == performType::init || (_analysisJaspResults && perform == performType::run)) ? changed : aborted;
	else
	{
		// the new analysis should be init or run (existing analyses will be aborted)
		_analysisId = analysisId;

		switch(perform)
		{
		case performType::init:		_analysisStatus = toInit;	break;
		case performType::run:		_analysisStatus = toRun;	break;
		case performType::saveImg:	_analysisStatus = saveImg;	break;
		case performType::editImg:	_analysisStatus = editImg;	break;
		default:					_analysisStatus = error;	break;
		}

	}

	if (_analysisStatus == toInit || _analysisStatus == toRun || _analysisStatus == changed || _analysisStatus == saveImg || _analysisStatus == editImg)
	{
		_analysisName			= jsonRequest.get("name",			Json::nullValue).asString();
		_analysisTitle			= jsonRequest.get("title",			Json::nullValue).asString();
		_analysisDataKey		= jsonRequest.get("dataKey",		Json::nullValue).toStyledString();
		_analysisOptions		= jsonRequest.get("options",		Json::nullValue).toStyledString();
		_analysisResultsMeta	= jsonRequest.get("resultsMeta",	Json::nullValue).toStyledString();
		_analysisStateKey		= jsonRequest.get("stateKey",		Json::nullValue).toStyledString();
		_analysisRevision		= jsonRequest.get("revision",		-1).asInt();
		_imageOptions			= jsonRequest.get("image",			Json::nullValue);
		_analysisRFile			= jsonRequest.get("rfile",				"").asString();
		_dynamicModuleCall		= jsonRequest.get("dynamicModuleCall",	"").asString();
		_analysisJaspResults	= _dynamicModuleCall != "" || jsonRequest.get("jaspResults",	false).asBool();
		_analysisRequiresInit	= jsonRequest.get("requiresInit",	Json::nullValue).isNull() ? true : jsonRequest.get("requiresInit", true).asBool();
		_ppi					= jsonRequest.get("ppi",			96).asInt();
		_imageBackground		= jsonRequest.get("imageBackground", "white").asString();

		_currentEngineState		= engineState::analysis;
	}
}

void Engine::runAnalysis()
{
#ifdef JASP_DEBUG
	std::cout << "Engine::runAnalysis()" << std::endl;
#endif
	if (_analysisStatus == saveImg)	{ saveImage(); return; }
	if (_analysisStatus == editImg)	{ editImage(); return; }

	if (_analysisStatus == empty || _analysisStatus == aborted)
	{
		_analysisStatus								= empty;
		_currentEngineState							= engineState::idle;
		return;
	}

	if (_analysisStatus == toInit && !_analysisJaspResults)	_analysisStatus = initing;
	else											_analysisStatus = running;

	std::string perform					= _analysisStatus == initing ? "init" : "run";

	RCallback callback					= boost::bind(&Engine::callback, this, _1, _2);

	_currentAnalysisKnowsAboutChange	= false;

	_analysisResultsString = _dynamicModuleCall != "" ?
			rbridge_runModuleCall(_analysisName, _analysisTitle, _dynamicModuleCall, _analysisDataKey, _analysisOptions, _analysisStateKey, perform, _ppi, _analysisId, _analysisRevision, _imageBackground)
		:	rbridge_run(_analysisName, _analysisTitle, _analysisRFile, _analysisRequiresInit, _analysisDataKey, _analysisOptions, _analysisResultsMeta, _analysisStateKey, _analysisId, _analysisRevision, perform, _ppi, _imageBackground, callback, _analysisJaspResults);

	if (_analysisStatus == initing || _analysisStatus == running)  // if status hasn't changed
		receiveMessages();

	if (_analysisStatus == toInit || _analysisStatus == aborted || _analysisStatus == error || _analysisStatus == exception)
	{
		// analysis was aborted, and we shouldn't send the results
		return;
	}
	else if (_analysisStatus == changed && (_currentAnalysisKnowsAboutChange == false || _analysisResultsString == "null"))
	{
		// analysis was changed, and the analysis either did not know about the change (because it did not call a callback),
		// or it could not incorporate the changes (returned null). In both cases it needs to be re-run, and results should not be sent

		_analysisStatus = toInit;

		if (_analysisResultsString == "null")
			TempFiles::deleteList(TempFiles::retrieveList(_analysisId));
		return;
	}
	else
	{
		Json::Reader().parse(_analysisResultsString, _analysisResults, false);

		if(!_analysisJaspResults)
		{
			_analysisStatus		= _analysisStatus == initing ? inited : complete;
			_progress	= -1;
			sendAnalysisResults();
		}

		_currentEngineState = engineState::idle;
		_analysisStatus		= empty;

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

	_analysisStatus										= complete;
	_analysisResults["results"]["inputOptions"]	= _imageOptions;
	_progress									= -1;
	sendAnalysisResults();

	_analysisStatus								= empty;
	_currentEngineState							= engineState::idle;
}

void Engine::editImage()
{
	std::string name	= _imageOptions.get("name", Json::nullValue).asString();
	std::string type	= _imageOptions.get("type", Json::nullValue).asString();
	int height			= _imageOptions.get("height", Json::nullValue).asInt();
	int width			= _imageOptions.get("width", Json::nullValue).asInt();
	std::string result	= jaspRCPP_editImage(name.c_str(), type.c_str(), height, width, _ppi);

	Json::Reader().parse(result, _analysisResults, false);

	_analysisStatus			= complete;
	_progress				= -1;
	sendAnalysisResults();

	_analysisStatus			= empty;
	_currentEngineState		= engineState::idle;
}

analysisResultStatus Engine::getStatusToAnalysisStatus()
{
	switch (_analysisStatus)
	{
	case inited:	return analysisResultStatus::inited;
	case running:
	case changed:	return analysisResultStatus::running;
	case complete:	return analysisResultStatus::complete;
	default:		return analysisResultStatus::error;
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

void Engine::removeNonKeepFiles(Json::Value filesToKeepValue)
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

	if (_analysisStatus == aborted || _analysisStatus == toInit || _analysisStatus == toRun)
		return "{ \"status\" : \"aborted\" }"; // abort

	if (_analysisStatus == changed && _currentAnalysisKnowsAboutChange)
	{
		_analysisStatus = running;
		_currentAnalysisKnowsAboutChange = false;
	}

	if (results != "null")
	{
		_analysisResultsString = results;

		Json::Reader().parse(_analysisResultsString, _analysisResults, false);

		_progress = progress;

		sendAnalysisResults();
	}
	else if (progress >= 0 && _analysisStatus == running)
	{
		_analysisResultsString	= "";
		_analysisResults		= Json::nullValue;
		_progress				= progress;

		sendAnalysisResults();

	}

	if (_analysisStatus == changed)
	{
		_currentAnalysisKnowsAboutChange = true; // because we're telling it now
		return "{ \"status\" : \"changed\", \"options\" : " + _analysisOptions + " }";
	}
	else if (_analysisStatus == aborted)	return "{ \"status\" : \"aborted\" }";

	return "{ \"status\" : \"ok\" }";
}

bool Engine::setColumnDataAsNominalOrOrdinal(bool isOrdinal, std::string columnName, std::vector<int> data, std::map<int, std::string> levels)
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

void Engine::pauseEngine()
{
	switch(_currentEngineState)
	{
	default:							/* everything not mentioned is fine */	break;
	case engineState::analysis:			_analysisStatus = aborted;				break;
	case engineState::filter:
	case engineState::computeColumn:	throw std::runtime_error("Unexpected data synch during " + engineStateToString(_currentEngineState) + " somehow, you should not expect to see this exception ever.");
	};

	_currentEngineState = engineState::paused;

	freeRBridgeColumns();
	SharedMemory::unloadDataSet();
	sendEnginePaused();
}

void Engine::resumeEngine()
{
	_currentEngineState = engineState::idle;
	sendEngineResumed();
}


void Engine::sendEnginePaused()
{
#ifdef JASP_DEBUG
	std::cout << "Engine paused" << std::endl;
#endif

	Json::Value rCodeResponse		= Json::objectValue;
	rCodeResponse["typeRequest"]	= engineStateToString(engineState::paused);

	sendString(rCodeResponse.toStyledString());
}

void Engine::sendEngineResumed()
{
#ifdef JASP_DEBUG
	std::cout << "Engine resuming" << std::endl;
#endif

	Json::Value rCodeResponse		= Json::objectValue;
	rCodeResponse["typeRequest"]	= engineStateToString(engineState::resuming);

	sendString(rCodeResponse.toStyledString());
}
