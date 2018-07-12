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

#include "../JASP-Common/analysisloader.h"
#include "../JASP-Common/tempfiles.h"
#include "../JASP-Common/utils.h"
#include "../JASP-Common/sharedmemory.h"

#include "rbridge.h"

void SendFunctionForJaspresults(const char * msg) { Engine::theEngine()->sendString(msg); }
bool PollMessagesFunctionForJaspResults()
{
	if(Engine::theEngine()->receiveMessages())
		return Engine::theEngine()->getStatus() == Engine::changed;
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

	tempfiles_attach(parentPID);

	rbridge_setDataSetSource(			boost::bind(&Engine::provideDataSet,				this));
	rbridge_setFileNameSource(			boost::bind(&Engine::provideTempFileName,			this, _1, _2, _3));
	rbridge_setStateFileSource(			boost::bind(&Engine::provideStateFileName,			this, _1, _2));
	rbridge_setJaspResultsFileSource(	boost::bind(&Engine::provideJaspResultsFileName,	this, _1, _2));

	rbridge_setColumnDataAsScaleSource(			boost::bind(&Engine::setColumnDataAsScale,			this, _1, _2));
	rbridge_setColumnDataAsOrdinalSource(		boost::bind(&Engine::setColumnDataAsOrdinal,		this, _1, _2));
	rbridge_setColumnDataAsNominalSource(		boost::bind(&Engine::setColumnDataAsNominal,		this, _1, _2));
	rbridge_setColumnDataAsNominalTextSource(	boost::bind(&Engine::setColumnDataAsNominalText,	this, _1, _2));

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

	while (ProcessInfo::isParentRunning())
	{
		receiveMessages(100);

		switch(_currentEngineState)
		{
		case engineState::idle:									break;
		case engineState::analysis:			runAnalysis();		break;
		default:
			std::cout << "Engine got stuck in engineState " << engineStateToString(_currentEngineState) << " which is not supposed to happen..." << std::endl;
		}
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
		case engineState::moduleRequest:	receiveModuleRequestMessage(jsonRequest);	break;
		default:							throw std::runtime_error("Engine::receiveMessages implement your new engineState!");
		}
	}

	return false;
}

void Engine::receiveFilterMessage(Json::Value jsonRequest)
{
	_currentEngineState			= engineState::filter;
	std::string filter			= jsonRequest.get("filter", "").asString();
	std::string generatedFilter = jsonRequest.get("generatedFilter", "").asString();

	runFilter(filter, generatedFilter);
}

void Engine::runFilter(std::string filter, std::string generatedFilter)
{
	try
	{
		std::vector<bool> filterResult	= rbridge_applyFilter(filter, generatedFilter);
		std::string RPossibleWarning	= jaspRCPP_getLastErrorMsg();

		sendFilterResult(filterResult, RPossibleWarning);

	}
	catch(filterException & e)
	{
		sendFilterError(std::string(e.what()).length() > 0 ? e.what() : "Something went wrong with the filter but it is unclear what.");
	}

	_currentEngineState = engineState::idle;
}

void Engine::sendFilterResult(std::vector<bool> filterResult, std::string warning)
{
	Json::Value filterResponse(Json::objectValue);

	filterResponse["typeRequest"]	= engineStateToString(engineState::filter);
	filterResponse["filterResult"]	= Json::arrayValue;

	for(bool f : filterResult)	filterResponse["filterResult"].append(f);
	if(warning != "")			filterResponse["filterError"] = warning;

	sendString(filterResponse.toStyledString());
}

void Engine::sendFilterError(std::string errorMessage)
{
	Json::Value filterResponse = Json::Value(Json::objectValue);

	filterResponse["typeRequest"] = engineStateToString(engineState::filter);
	filterResponse["filterError"] = errorMessage;

	sendString(filterResponse.toStyledString());
}

void Engine::receiveRCodeMessage(Json::Value jsonRequest)
{
	_currentEngineState	= engineState::rCode;
	std::string rCode	= jsonRequest.get("rCode", "").asString();
	int rCodeRequestId	= jsonRequest.get("requestId", -1).asInt();

	runRCode(rCode, rCodeRequestId);
}

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
	_currentEngineState = engineState::computeColumn;

	std::string			computeColumnName = jsonRequest.get("columnName", "").asString();
	std::string			computeColumnCode = jsonRequest.get("computeCode", "").asString();
	Column::ColumnType	computeColumnType = Column::columnTypeFromString(jsonRequest.get("columnType", "").asString());

	runComputeColumn(computeColumnName, computeColumnCode, computeColumnType);
}

void Engine::runComputeColumn(std::string computeColumnName, std::string computeColumnCode, Column::ColumnType computeColumnType)
{
	static const std::map<Column::ColumnType, std::string> setColumnFunction = {{Column::ColumnTypeScale,".setColumnDataAsScale"}, {Column::ColumnTypeOrdinal,".setColumnDataAsOrdinal"}, {Column::ColumnTypeNominal,".setColumnDataAsNominal"}, {Column::ColumnTypeNominalText,".setColumnDataAsNominalText"}};

	std::string computeColumnCodeComplete	= "calcedVals <- {"+computeColumnCode +"};\n" + setColumnFunction.at(computeColumnType) + "('" + computeColumnName +"', calcedVals);\n return('succes');";
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
	bool			fail			= result == "null";

	Json::Value		jsonAnswer		= Json::objectValue;

	jsonAnswer["moduleRequest"]		= moduleRequest;
	jsonAnswer["moduleName"]		= moduleName;
	jsonAnswer["succes"]			= !fail;
	jsonAnswer["error"]				= jaspRCPP_getLastErrorMsg();

	sendString(jsonAnswer.toStyledString());

	_currentEngineState = engineState::idle;
}


//Analysis stuff:

void Engine::receiveAnalysisMessage(Json::Value jsonRequest)
{
#ifdef PRINT_ENGINE_MESSAGES
	std::cout << jsonRequest.toStyledString() << std::endl;
	std::cout.flush();
#endif

	int analysisId		= jsonRequest.get("id", -1).asInt();
	performType perform	= performTypeFromString(jsonRequest.get("perform", "run").asString());

	if (analysisId == _analysisId && _status == running)
	{
		// if the current running analysis has changed
		if (perform == performType::init || (_analysisJaspResults && perform == performType::run))
			_status = changed;
		else
			_status = aborted;
	}
	else
	{
		// the new analysis should be init or run (existing analyses will be aborted)
		_analysisId = analysisId;

		switch(perform)
		{
		case performType::init:		_status = toInit;	break;
		case performType::run:		_status = toRun;	break;
		case performType::saveImg:	_status = saveImg;	break;
		case performType::editImg:	_status = editImg;	break;
		default:					_status = error;	break;
		}

	}

	if (_status == toInit || _status == toRun || _status == changed || _status == saveImg || _status == editImg)
	{
		_analysisName			= jsonRequest.get("name",			Json::nullValue).asString();
		_analysisTitle			= jsonRequest.get("title",			Json::nullValue).asString();
		_analysisDataKey		= jsonRequest.get("dataKey",		Json::nullValue).toStyledString();
		_analysisOptions		= jsonRequest.get("options",		Json::nullValue).toStyledString();
		_analysisResultsMeta	= jsonRequest.get("resultsMeta",	Json::nullValue).toStyledString();
		_analysisStateKey		= jsonRequest.get("stateKey",		Json::nullValue).toStyledString();
		_analysisRevision		= jsonRequest.get("revision",		-1).asInt();
		_imageOptions			= jsonRequest.get("image",			Json::nullValue);
		_rfile					= jsonRequest.get("rfile",			Json::nullValue).asString();
		_analysisJaspResults	= jsonRequest.get("jaspResults",	false).asBool();
		_analysisRequiresInit	= jsonRequest.get("requiresInit", Json::nullValue).isNull() ? true : jsonRequest.get("requiresInit", true).asBool();
		_ppi					= jsonRequest.get("ppi",			96).asInt();

		_currentEngineState = engineState::analysis;
	}
}

void Engine::runAnalysis()
{
	if (_status == saveImg)	{ saveImage(); return; }
	if (_status == editImg)	{ editImage(); return; }

	if (_status == empty || _status == aborted)
		return;

	if (_status == toInit && !_analysisJaspResults)	_status = initing;
	else											_status = running;

	std::string perform = _status == initing ? "init" : "run";


	RCallback callback					= boost::bind(&Engine::callback, this, _1, _2);

	_currentAnalysisKnowsAboutChange	= false;
	_analysisResultsString				= rbridge_run(_analysisName, _analysisTitle, _rfile, _analysisRequiresInit, _analysisDataKey, _analysisOptions, _analysisResultsMeta, _analysisStateKey, _analysisId, _analysisRevision, perform, _ppi, callback, _analysisJaspResults);

	if (_status == initing || _status == running)  // if status hasn't changed
		receiveMessages();

	if (_status == toInit || _status == aborted || _status == error || _status == exception)
	{
		// analysis was aborted, and we shouldn't send the results
		return;
	}
	else if (_status == changed && (_currentAnalysisKnowsAboutChange == false || _analysisResultsString == "null"))
	{
		// analysis was changed, and the analysis either did not know about the change (because it did not call a callback),
		// or it could not incorporate the changes (returned null). In both cases it needs to be re-run, and results should not be sent

		_status = toInit;

		if (_analysisResultsString == "null")
			tempfiles_deleteList(tempfiles_retrieveList(_analysisId));
		return;
	}
	else
	{
		Json::Reader().parse(_analysisResultsString, _analysisResults, false);

		if(!_analysisJaspResults)
		{
			_status		= _status == initing ? inited : complete;
			_progress	= -1;
			sendAnalysisResults();
		}

		_currentEngineState = engineState::idle;
		_status		= empty;
		removeNonKeepFiles(_analysisResults.isObject() ? _analysisResults.get("keep", Json::nullValue) : Json::nullValue);

	}
}

void Engine::saveImage()
{
	std::string name	= _imageOptions.get("name", Json::nullValue).asString();
	std::string type	= _imageOptions.get("type", Json::nullValue).asString();
	int height			= _imageOptions.get("height", Json::nullValue).asInt();
	int width			= _imageOptions.get("width", Json::nullValue).asInt();

	std::string result = jaspRCPP_saveImage(name.c_str(), type.c_str(), height, width, _ppi);

	Json::Reader().parse(result, _analysisResults, false);

	_status										= complete;
	_analysisResults["results"]["inputOptions"]	= _imageOptions;
	_progress									= -1;
	sendAnalysisResults();
	_status										= empty;
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

	_status				= complete;
	_progress			= -1;
	sendAnalysisResults();
	_status				= empty;
	_currentEngineState	= engineState::idle;
}

analysisResultStatus Engine::getStatusToAnalysisStatus()
{
	switch (_status)
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

	std::vector<std::string> tempFilesFromLastTime = tempfiles_retrieveList(_analysisId);

	Utils::remove(tempFilesFromLastTime, filesToKeep);

	tempfiles_deleteList(tempFilesFromLastTime);
}


DataSet * Engine::provideDataSet()
{
	return SharedMemory::retrieveDataSet(_parentPID);
}

void Engine::provideStateFileName(std::string &root, std::string &relativePath)
{
	return tempfiles_createSpecific("state", _analysisId, root, relativePath);
}

void Engine::provideJaspResultsFileName(std::string &root, std::string &relativePath)
{
	return tempfiles_createSpecific("jaspResults.json", _analysisId, root, relativePath);
}

void Engine::provideTempFileName(const std::string &extension, std::string &root, std::string &relativePath)
{
	tempfiles_create(extension, _analysisId, root, relativePath);
}


std::string Engine::callback(const std::string &results, int progress)
{
	receiveMessages();

	if (_status == aborted || _status == toInit || _status == toRun)
		return "{ \"status\" : \"aborted\" }"; // abort

	if (_status == changed && _currentAnalysisKnowsAboutChange)
	{
		_status = running;
		_currentAnalysisKnowsAboutChange = false;
	}

	if (results != "null")
	{
		_analysisResultsString = results;

		Json::Reader().parse(_analysisResultsString, _analysisResults, false);

		_progress = progress;

		sendAnalysisResults();
	}
	else if (progress >= 0 && _status == running)
	{
		_analysisResultsString	= "";
		_analysisResults		= Json::nullValue;
		_progress				= progress;

		sendAnalysisResults();

	}

	if (_status == changed)
	{
		_currentAnalysisKnowsAboutChange = true; // because we're telling it now
		return "{ \"status\" : \"changed\", \"options\" : " + _analysisOptions + " }";
	}
	else if (_status == aborted)	return "{ \"status\" : \"aborted\" }";

	return "{ \"status\" : \"ok\" }";
}
