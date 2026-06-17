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

#include "log.h"
#include "utils.h"
#include "engine.h"
#include "timers.h"
#include "rbridge.h"
#include "tempfiles.h"
#include "columnutils.h"
#include "utilities/qutils.h"
#include "databaseinterface.h"
#include "r_functionwhitelist.h"

void SendFunctionForJaspresults(const char * msg) 
{
	Json::Reader	parser;
	Json::Value		json;
	
	if(parser.parse(msg, json))
		Engine::theEngine()->sendString(json); 
	else
		Engine::theEngine()->sendString(msg); 

}

bool PollMessagesFunctionForJaspResults()
{
	if(Engine::theEngine()->receiveMessages())
	{
		if(Engine::theEngine()->paused())
			return true;
		else
			switch(Engine::theEngine()->getAnalysisStatus())
			{
			case engineAnalysisStatus::changed:
			case engineAnalysisStatus::aborted:
			case engineAnalysisStatus::stopped:
			Log::log() << "Analysis status changed for engine #" << Engine::theEngine()->engineNum() << " to: " << engineAnalysisStatusToString(Engine::theEngine()->getAnalysisStatus()) << std::endl;
				return true;
				
			default:							
				break;
			}
	}
	return false;
}

#ifdef _WIN32

#undef Realloc
#undef Free

#endif

Engine * Engine::_EngineInstance = NULL;

Engine::Engine(int slaveNo, unsigned long parentPID)
	: DataBridge(parentPID), _engineNum(slaveNo), _parentPID(parentPID)
{
	JASPTIMER_SCOPE(Engine Constructor);
	assert(_EngineInstance == NULL);
	_EngineInstance = this;
}

void Engine::initialize()
{
	Log::log() << "Engine::initialize()" << std::endl;

	try
	{
		std::string memoryName = "JASP-IPC-" + std::to_string(_parentPID);
		_channel = new IPCChannel(memoryName, _engineNum, true);

		rbridge_init(this, SendFunctionForJaspresults, PollMessagesFunctionForJaspResults, _resultFont.c_str());

		Log::log() << "rbridge_init completed" << std::endl;
		
		_datasetProvidedCallback = [this]()
		{
			if(_engineState == engineState::reloadData)
			{
				sendEngineResumed();
				_engineState = engineState::idle;
			}
		};
	
		sendEngineLoadingData();
	}
	catch(std::exception & e)
	{
		Log::log() << "Engine::initialize() failed! The exception caught was: '" << e.what() << "'" << std::endl;
		throw e;
	}
}

Engine::~Engine()
{
	delete _channel; //shared memory files will be removed in jaspDesktop
	_channel = nullptr;
}

bool Engine::parentAlive()
{
	return _channel->jaspAlive();
}

void Engine::run()
{
	do
	{
		static bool initDone = false;
		if(!initDone && _engineState == engineState::initializing) //Do this first, otherwise receiveMessages possibly triggers some other functions
		{
			initialize();
			initDone = true;
		}

		receiveMessages(100);

		switch(_engineState)
		{

		case engineState::idle:				beIdle(_lastRequest == engineState::analysis);	break;
		case engineState::analysis:			runAnalysis();									break;
		case engineState::initializing:
		case engineState::paused:			/* Do nothing */
		case engineState::stopped:															break;
		case engineState::resuming:			throw std::runtime_error("Enginestate " + engineStateToString(_engineState) + " should NOT be set as currentState!");
		case engineState::reloadData:		provideAndUpdateDataSet();						break;
		default:
			Log::log() << "Engine got stuck in engineState " << engineStateToString(_engineState) << " which is not supposed to happen..." << std::endl;
		}
	}
	while(_engineState != engineState::stopped && parentAlive());

	if(_engineState == engineState::stopped)
		Log::log() << "Engine leaving mainloop after having been asked to stop." << std::endl;
	
	if(!parentAlive())
	{
		Log::log() << "Engine leaving mainloop after having no parent alive, sending engine stopped just in case we just awoke from sleep." << std::endl;
		sendEngineStopped();
	}

	delete _channel;
	_channel = nullptr;
}

void Engine::beIdle(bool newlyIdle)
{
	static int64_t idleStartTime = -1;

	if(newlyIdle)
		idleStartTime = Utils::currentSeconds();
	else if(idleStartTime != -1 && idleStartTime + 10 < Utils::currentSeconds())
	{
		Log::log() << "Attempting to clean up memory used by engine/R a bit." << std::endl;
		rbridge_memoryCleaning();
		Log::log() << "Memory is cleaned up" << std::endl;
		idleStartTime = -1;
	}

	_lastRequest = engineState::idle;
}

bool Engine::receiveMessages(int timeout)
{
	std::string data;
	if (_channel->receive(data, timeout))
	{
		if(data == "")
		{
			Log::log() << "Received nothing..." << std::endl;
			return false;
		}

		// JSONCPP_STRING          err;
		// Json::Value		        jsonRequest;
		// Json::CharReaderBuilder jsonReaderBuilder;
		// std::unique_ptr<Json::CharReader> const jsonReader(jsonReaderBuilder.newCharReader());

		// if(!jsonReader->parse(data.c_str(), data.c_str() + data.length(), &jsonRequest, &err))
		

		Json::Value		jsonRequest;
		Json::Reader	jsonReader;

		if(!jsonReader.parse(data, jsonRequest, false))
		{
			Log::log() << "Engine got request:\nrow 0:\t";

			size_t row=0;
			for(const char & c : data)
			{
				if(c == '\n')
					Log::log() << "\nrow " << ++row << ":\t";
				Log::log() << c;
			}

			Log::log() << "Parsing request failed on:\n" << jsonReader.getFormattedErrorMessages() << std::endl;
			// Log::log() << "Parsing request failed on:\n" << err << std::endl;
		}

		//Clear send buffer and anonymized log
		Json::Value printData;
		bool parsed = jsonReader.parse(data, printData);
		if (parsed && printData.isMember("GITHUB_PAT")) {
			printData["GITHUB_PAT"] = "********";
		}
		
		//Log::log() << "Received: '" << printData.toStyledString() << "' so now clearing my send buffer" << std::endl;

		sendString("");

		//Check if we got anyting useful
		std::string typeSend	= jsonRequest.get("typeRequest", Json::nullValue).asString();
		if(typeSend == "")
		{
			Log::log() << "It seems the required field \"typeRequest\" was empty: '" << typeSend << "'" << std::endl;
			return false;
		}

		_lastRequest = engineStateFromString(typeSend);

#ifdef PRINT_ENGINE_MESSAGES
		Log::log() << "Engine received " << engineStateToString(_lastRequest) <<" message" << std::endl;
#endif

		if(_engineState == engineState::initializing)
		{
			if(_lastRequest == engineState::resuming)
				resumeEngine(jsonRequest);
			//We ignore everything else
		}
		else
			switch(_lastRequest)
			{
			case engineState::analysis:				receiveAnalysisMessage(jsonRequest);		return true;
			case engineState::filter:				receiveFilterMessage(jsonRequest);			break;
			case engineState::filterByName:			receiveFilterByNameMessage(jsonRequest);	break;
			case engineState::rCode:				receiveRCodeMessage(jsonRequest);			break;
			case engineState::computeColumn:		receiveComputeColumnMessage(jsonRequest);	break;
			case engineState::pauseRequested:		pauseEngine(jsonRequest);					break;
			case engineState::resuming:				resumeEngine(jsonRequest);					break;
			case engineState::moduleUninstallRequest:
			case engineState::moduleInstallRequest:
			case engineState::moduleLoadRequest:	receiveModuleRequestMessage(jsonRequest);	break;
			case engineState::stopRequested:		stopEngine();								break;
			case engineState::logCfg:				receiveLogCfg(jsonRequest);					break;
			case engineState::settings:				receiveSettings(jsonRequest);				break;
			case engineState::reloadData:			receiveReloadData();						break;
			default:								throw std::runtime_error("Engine::receiveMessages begs you to add your new engineState " + engineStateToString(_lastRequest) + " to it!");
			}
	}

	return false;
}

void Engine::receiveFilterMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		Log::log() << "Unexpected filter message, current state is not idle (" << engineStateToString(_engineState) << ")";

	_engineState				= engineState::filter;
	std::string filter			= jsonRequest.get("filter", "").asString();
	std::string generatedFilter = jsonRequest.get("generatedFilter", "").asString();
	int filterRequestId			= jsonRequest.get("requestId", -1).asInt();

	runFilter(filter, generatedFilter, filterRequestId);
}

void Engine::receiveFilterByNameMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		Log::log() << "Unexpected filterByName message, current state is not idle (" << engineStateToString(_engineState) << ")";

	_engineState				= engineState::filter;
	std::string name			= jsonRequest.get("name", "").asString();

	runFilterByName(name);
}

void Engine::runFilterByName(const std::string & name)
{
	provideAndUpdateDataSet();
	
	Filter		localFilter			(_dataSet, name, false);
	std::string strippedFilter		= stringUtils::stripRComments(localFilter.rFilter());
	boolvec		filterResult;
	std::string RPossibleWarning;
	try
	{
		filterResult		= rbridge_applyFilter(strippedFilter, "");
		RPossibleWarning	= jaspRCPP_getLastErrorMsg();
	}
	catch(filterException & e)
	{
		std::string error = std::string(e.what()).length() > 0 ? e.what() : "but it is unclear what the problem was...";
		error = "There was a problem running filter '" + name + "':\n" + error;
		Log::log() << error << std::endl;
		
		filterResult		= boolvec(_dataSet ? _dataSet->rowCount() : 0, false); //in the case of a non-dataset filter a better default is probably better to set everything to false if something is wrong
		RPossibleWarning	= error;
	}
	


	DatabaseInterface::singleton()->transactionWriteBegin();
	localFilter.setFilterVector(filterResult);
	localFilter.setErrorMsg(RPossibleWarning);
	localFilter.incRevision();
	DatabaseInterface::singleton()->transactionWriteEnd();

	sendFilterByNameDone(name, RPossibleWarning);

	_engineState = engineState::idle;
}



void Engine::runFilter(const std::string & filter, const std::string & generatedFilter, int filterRequestId)
{
	try
	{		
		
		
		std::string		strippedFilter		= stringUtils::stripRComments(filter);
		boolvec			filterResult		= rbridge_applyFilter(strippedFilter, generatedFilter);
		std::string		RPossibleWarning	= jaspRCPP_getLastErrorMsg();

		Log::log() << "Engine::runFilter ran:\n\t" << strippedFilter << "\n\tRPossibleWarning='" << RPossibleWarning << "'\n\t\tfor revision " << _dataSet->filter()->revision() << std::endl;

		_dataSet->db().transactionWriteBegin();
		_dataSet->filter()->setRFilter(filter);
		_dataSet->filter()->setFilterVector(filterResult);
		_dataSet->filter()->setErrorMsg(RPossibleWarning);
		_dataSet->filter()->incRevision();
		_dataSet->db().transactionWriteEnd();


		sendFilterResult(filterRequestId);

	}
	catch(filterException & e)
	{
		std::string error = std::string(e.what()).length() > 0 ? e.what() : "Something went wrong with the filter but it is unclear what.";
		
		while(_dataSet->db().transactionReadDepth() > 0)
			_dataSet->db().transactionReadEnd();
		
		_dataSet->db().transactionWriteBegin();
		_dataSet->filter()->setErrorMsg(error);
		_dataSet->filter()->incRevision();
		_dataSet->db().transactionWriteEnd();

		sendFilterError(filterRequestId, error);
	}

	_engineState = engineState::idle;
}

void Engine::sendFilterResult(int filterRequestId)
{
	Json::Value filterResponse(Json::objectValue);

	filterResponse["typeRequest"]	= engineStateToString(engineState::filter);
	filterResponse["requestId"]		= filterRequestId;

	sendString(filterResponse);
}

void Engine::sendFilterError(int filterRequestId, const std::string & errorMessage)
{
	Json::Value filterResponse = Json::Value(Json::objectValue);

	Log::log() << "Engine::sendFilterError(filterRequestId=" << filterRequestId << ", errorMsg='" << errorMessage << "')" << std::endl;

	filterResponse["typeRequest"]	= engineStateToString(engineState::filter);
	filterResponse["requestId"]		= filterRequestId;
	filterResponse["error"]			= errorMessage;

	sendString(filterResponse);
}

void Engine::sendFilterByNameDone(const std::string & name, const std::string & errorMessage)
{
	Json::Value filterResponse(Json::objectValue);

	filterResponse["typeRequest"]	= engineStateToString(engineState::filterByName);
	filterResponse["name"]			= name;
	filterResponse["errorMessage"]	= errorMessage;

	sendString(filterResponse);
}

void Engine::receiveRCodeMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		Log::log() << "Unexpected rCode message, current state is not idle (" << engineStateToString(_engineState) << ")";

				_engineState	= engineState::rCode;
	std::string rCode			= jsonRequest.get("rCode",			"").asString();
	int			rCodeRequestId	= jsonRequest.get("requestId",		-1).asInt();
	bool		whiteListed		= jsonRequest.get("whiteListed",	true).asBool(),
				returnLog		= jsonRequest.get("returnLog",		false).asBool();

	if(returnLog)	runRCodeCommander(rCode);
	else			runRCode(rCode, rCodeRequestId, whiteListed);
}

// Evaluating arbitrary R code (as string) which returns a string
void Engine::runRCode(const std::string & rCode, int rCodeRequestId, bool whiteListed)
{
	provideAndUpdateDataSet();
	

	std::string rCodeResult = whiteListed ? rbridge_evalRCodeWhiteListed(rCode.c_str(), true) : jaspRCPP_evalRCode(rCode.c_str(), true);
	bool		hadError	= rCodeResult == "null";

	if (hadError)	sendRCodeError(rCodeRequestId);
	else			sendRCodeResult(rCodeRequestId, rCodeResult);

	_engineState = engineState::idle;
}


void Engine::runRCodeCommander(std::string rCode)
{
    bool thereIsSomeData = provideAndUpdateDataSet() && provideAndUpdateDataSet()->rowCount();


	static const std::string rCmdDataName = "data", rCmdFiltered = "filteredData";


	if(thereIsSomeData)
	{
		
		
		rCode = ColumnEncoder::encodeAll(rCode);
		jaspRCPP_runScript((rCmdDataName + "<- .readFullDatasetToEnd();").c_str());
		jaspRCPP_runScript((rCmdFiltered + "<- .readFullFilteredDatasetToEnd();").c_str());
	}

	std::string rCodeResult =	jaspRCPP_evalRCodeCommander(rCode.c_str());

	if(thereIsSomeData)
	{
		rbridge_detachRCodeEnv(rCmdFiltered);
		rbridge_detachRCodeEnv(rCmdDataName);
		rCodeResult = ColumnEncoder::decodeAll(rCodeResult);
	}

	sendRCodeResult(-1, rCodeResult);

	_engineState = engineState::idle;
}


void Engine::sendRCodeResult(int rCodeRequestId, const std::string & rCodeResult)
{
	Json::Value rCodeResponse(Json::objectValue);

	std::string RError				= jaspRCPP_getLastErrorMsg();
	if(RError.size() > 0)
		rCodeResponse["rCodeError"]	= RError;

	rCodeResponse["typeRequest"]	= engineStateToString(engineState::rCode);
	rCodeResponse["rCodeResult"]	= rCodeResult;
	rCodeResponse["requestId"]		= rCodeRequestId;


	sendString(rCodeResponse);
}

void Engine::sendRCodeError(int rCodeRequestId)
{
	Log::log() << "R Code yielded error" << std::endl;

	Json::Value rCodeResponse		= Json::objectValue;
	std::string RError				= jaspRCPP_getLastErrorMsg();
	rCodeResponse["typeRequest"]	= engineStateToString(engineState::rCode);
	rCodeResponse["rCodeError"]		= RError.size() == 0 ? "R Code failed for unknown reason. Check that R function returns a string." : RError;
	rCodeResponse["requestId"]		= rCodeRequestId;

	sendString(rCodeResponse);
}

void Engine::receiveComputeColumnMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		Log::log() << "Unexpected compute column message, current state is not idle (" << engineStateToString(_engineState) << ")";

	_engineState = engineState::computeColumn;

	std::string	computeColumnName =						 jsonRequest.get("columnName",  "").asString();
	std::string	computeColumnCode =						 jsonRequest.get("computeCode", "").asString();
	columnType	computeColumnType = columnTypeFromString(jsonRequest.get("columnType",  "").asString());

	runComputeColumn(computeColumnName, computeColumnCode, computeColumnType);
}

void Engine::runComputeColumn(const std::string & computeColumnName, const std::string & computeColumnCode, columnType computeColumnType)
{
	Log::log() << "Engine::runComputeColumn()" << std::endl;

	static const std::map<columnType, std::string> setColumnFunction = {
		{columnType::scale,			".setColumnDataAsScale"			},
		{columnType::ordinal,		".setColumnDataAsOrdinal"		},
		{columnType::nominal,		".setColumnDataAsNominal"		},
		{columnType::nominalText,	".setColumnDataAsNominalText"	}};

	Json::Value computeColumnResponse		= Json::objectValue;
	computeColumnResponse["typeRequest"]	= engineStateToString(engineState::computeColumn);
	computeColumnResponse["columnName"]		= computeColumnName;
	
    if(provideAndUpdateDataSet())
	{
		try
		{
			std::string computeColumnNameEnc = ColumnEncoder::columnEncoder()->encode(computeColumnName);
			computeColumnResponse["columnName"]		= computeColumnNameEnc;
			
			Column * compCol = _dataSet->column(computeColumnName);
			
			

			std::string useThisFilter				= compCol->computeFilter(),
						computeColumnResultStr		= rbridge_evalRComputedColumn(
							computeColumnCode, 
							"toString("+ setColumnFunction.at(computeColumnType) + "('" + computeColumnNameEnc +"', .calcedVals, 1))",
							useThisFilter);
	
			computeColumnResponse["result"]			= computeColumnResultStr;
			computeColumnResponse["error"]			= jaspRCPP_getLastErrorMsg();
		}
		catch(std::exception e)
		{
			throw e;
		}
	}
	else
	{
		computeColumnResponse["result"]			= "fail";
		computeColumnResponse["error"]			= "No DataSet loaded in engine!";
	}

	sendString(computeColumnResponse);
	
	_engineState = engineState::idle;
}

void Engine::receiveModuleRequestMessage(const Json::Value & jsonRequest)
{
	_engineState					= engineStateFromString(jsonRequest.get("typeRequest", Json::nullValue).asString());

	std::string		moduleRequest	= jsonRequest["moduleRequest"].asString();
	std::string		moduleCode		= jsonRequest["moduleCode"].asString();
	std::string		moduleName		= jsonRequest["moduleName"].asString();
	std::string		moduleLibPaths  = jsonRequest["moduleLibPaths"].asString();

	Log::log() << "About to run module request for module '" << moduleName << "' and code to run:\n'" << moduleCode << "'" << std::endl;

	if(moduleStatusFromString((moduleRequest)) == moduleStatus::loading) {
		//Some jaspModules use jaspBase calls in their .onload so we first we need to prepare jaspbase
		jaspRCPP_evalRCode((".libPaths( " + moduleLibPaths +  " ); 'success'").c_str(), false);
		jaspRCPP_init_jaspBase();
	}

	std::string		result			= jaspRCPP_evalRCode(moduleCode.c_str(), false);
	bool			succes			= result == "succes!"; //Defined in DynamicModule::succesResultString()

	if(moduleStatusFromString((moduleRequest)) == moduleStatus::installNeeded || moduleStatusFromString((moduleRequest)) == moduleStatus::uninstallNeeded)
		succes = (result.find("null") == std::string::npos);

	Log::log() << "Was " << (succes ? "succesful" : "a failure") << ", now crafting answer." << std::endl;

	Json::Value		jsonAnswer		= Json::objectValue;

	jsonAnswer["moduleRequest"]		= moduleRequest;
	jsonAnswer["moduleName"]		= moduleName;
	jsonAnswer["succes"]			= succes;
	jsonAnswer["error"]				= jaspRCPP_getLastErrorMsg();
	jsonAnswer["typeRequest"]		= engineStateToString(_engineState);

	if(moduleStatusFromString((moduleRequest)) == moduleStatus::installNeeded)
		jsonAnswer["result"] = result;

	if(!succes)
		Log::log() << "Error was:\n" << jsonAnswer["error"].asString() << std::endl;

	Log::log() << "Sending it." << std::endl;

	sendString(jsonAnswer);

	_engineState = engineState::idle;
}

void Engine::receiveAnalysisMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle && _engineState != engineState::analysis)
		throw std::runtime_error("Unexpected analysis message, current state is not idle or analysis (" + engineStateToString(_engineState) + ")");

	int analysisId		= jsonRequest.get("id",			-1).asInt();
	performType perform	= performTypeFromString(jsonRequest.get("perform", "run").asString());
	
#ifdef PRINT_ENGINE_MESSAGES
	Log::log() << "Engine::receiveAnalysisMessage:\n" << jsonRequest.toStyledString() << " while current analysisStatus is: " << engineAnalysisStatusToString(_analysisStatus) << "\n";
#endif

	if (analysisId == _analysisId && _analysisStatus == Status::running)
	{
		Log::log() << "Currently running analysis changed option, " << (perform == performType::run ? " it's status will become changed because a new run is requested." : " it will be aborted because the new request isn't toRun.") << std::endl;
		// if the current running analysis has changed
		_analysisStatus = perform == performType::run ? Status::changed : Status::aborted;
	}
	else
	{
		// the new analysis should be init or run (existing analyses will be aborted)
		Log::log() << "It is either not the same analysis or the current one isn't \"running\", so the new one will do: ";
		_analysisId = analysisId;

		switch(perform)
		{
		case performType::run:			_analysisStatus = Status::toRun;		break;
		case performType::saveImg:		_analysisStatus = Status::saveImg;		break;
		case performType::editImg:		_analysisStatus = Status::editImg;		break;
		case performType::rewriteImgs:	_analysisStatus = Status::rewriteImgs;	break;
		case performType::abort:		_analysisStatus = Status::aborted;		break;
		default:						_analysisStatus = Status::error;		break;
		}
		
		Log::log(false) << engineAnalysisStatusToString(_analysisStatus) << std::endl;

	}

#ifdef PRINT_ENGINE_MESSAGES
	Log::log() << "msg type was '" << engineAnalysisStatusToString(_analysisStatus) << "'" << std::endl;
#endif

	if(	_analysisStatus == Status::toRun		||
		_analysisStatus == Status::changed		||
		_analysisStatus == Status::saveImg		||
		_analysisStatus == Status::editImg		||
		_analysisStatus == Status::rewriteImgs	  )
	{
		Log::log() << "Loading new settings for analysis ";
		
		_analysisName			= jsonRequest.get("name",				Json::nullValue).asString();
		_analysisTitle			= jsonRequest.get("title",				Json::nullValue).asString();
		_analysisDataKey		= jsonRequest.get("dataKey",			Json::nullValue).toStyledString();
		_analysisResultsMeta	= jsonRequest.get("resultsMeta",		Json::nullValue).toStyledString();
		_analysisStateKey		= jsonRequest.get("stateKey",			Json::nullValue).toStyledString();
		_analysisRevision		= jsonRequest.get("revision",			-1).asInt();
		_imageOptions			= jsonRequest.get("image",				Json::nullValue);
		_analysisRFile			= jsonRequest.get("rfile",				"").asString();
		_dynamicModuleCall		= jsonRequest.get("dynamicModuleCall",	"").asString();
		_resultFont				= jsonRequest.get("resultFont",			"").asString();
		_analysisPreloadData	= jsonRequest.get("preloadData",		false).asBool();
		_engineState			= engineState::analysis;

		Json::Value optionsEnc	= jsonRequest.get("options",			Json::nullValue);
		
		Log::log(false) << _analysisTitle << " with ID " << _analysisId << std::endl;
		
		extraEncodings()->setCurrentNamesFromOptionsMeta(optionsEnc);
		
		_analysisOptions		= optionsEnc; //store unencoded
	}
	// No need to check else for aborted because PollMessagesFunctionForJaspResults will pass that msg on by itself.
}


void Engine::sendString(Json::Value message)
{
	std::string msgStr;
	
	if(message.isObject()) //If everything is converted to jaspResults maybe we can do this there?
	{
		ColumnEncoder::columnEncoder()->decodeJsonSafeHtml(message); // decode all columnnames as far as you can
		msgStr = message.toStyledString();
	}
	else if(message.isString())
		msgStr = message.asString();
	
	ColumnUtils::convertEscapedUnicodeToUTF8(msgStr);
	
	_channel->send(msgStr);
}


void Engine::runAnalysis()
{
	Log::log() << "Engine::runAnalysis() " << _analysisTitle << " (" << _analysisId << ") revision: " << _analysisRevision << std::endl;

	switch(_analysisStatus)
	{
	case Status::saveImg:		 saveImage();						return;
	case Status::editImg:		 editImage();						return;
	case Status::rewriteImgs:	 rewriteImages();					return;

	case Status::empty:
	case Status::aborted:
		_analysisStatus	= Status::empty;
		_engineState	= engineState::idle;
		Log::log() << "Engine::state <= idle because it does not need to be run now (empty || aborted)" << std::endl;
		return;

	default:	break;
	}

	provideAndUpdateDataSet();
	Log::log() << "Analysis will be run now." << std::endl;

	Json::Value encodedAnalysisOptions = _analysisOptions;
	
	updateOptionsAccordingToMeta(encodedAnalysisOptions);

	_analysisColsTypes = ColumnEncoder::encodeColumnNamesinOptions(encodedAnalysisOptions, _analysisPreloadData);

	//Log::log() << "Encoded options right before calling R are:\n" << encodedAnalysisOptions << std::endl;

	_analysisResultsString = rbridge_runModuleCall(_analysisName, _analysisTitle, _dynamicModuleCall, _analysisDataKey,
								encodedAnalysisOptions.toStyledString(), _analysisStateKey, _analysisId, _analysisRevision, 
								_developerMode, _analysisColsTypes, _analysisPreloadData);

	switch(_analysisStatus)
	{
	case Status::aborted:
	case Status::error:
	case Status::exception:
		return;

	case Status::changed: 
			// analysis was changed, and the analysis killed itself through jaspResults::checkForAnalysisChanged()
			//It needs to be re-run and the tempfiles can be cleared.
			_analysisStatus = Status::toRun;
			TempFiles::deleteList(TempFiles::retrieveList(_analysisId));
			return;
		

	default: 

			// JSONCPP_STRING          err;
			// Json::CharReaderBuilder jsonReaderBuilder;
			// std::unique_ptr<Json::CharReader> const jsonReader(jsonReaderBuilder.newCharReader());

			// jsonReader->parse(_analysisResultsString.c_str(), _analysisResultsString.c_str() + _analysisResultsString.length(), &_analysisResults, &err);

			Json::Reader().parse(_analysisResultsString, _analysisResults, false);

			_engineState	= engineState::idle;
			_analysisStatus = Status::empty;

			removeNonKeepFiles(_analysisResults.isObject() ? _analysisResults.get("keep", Json::nullValue) : Json::nullValue);
			return;
		
	}
}

void Engine::saveImage()
{	
	int			height	= _imageOptions.get("height",	Json::nullValue).asInt(),
				width	= _imageOptions.get("width",	Json::nullValue).asInt();
	std::string data	= _imageOptions.get("data",		Json::nullValue).asString(),
				type	= _imageOptions.get("type",		Json::nullValue).asString(),
				result	= jaspRCPP_saveImage(data.c_str(), type.c_str(), height, width);

	Json::Reader().parse(result, _analysisResults, false);

	// JSONCPP_STRING          err;
	// Json::CharReaderBuilder jsonReaderBuilder;
	// std::unique_ptr<Json::CharReader> const jsonReader(jsonReaderBuilder.newCharReader());

	// jsonReader->parse(result.c_str(), result.c_str() + result.length(), &_analysisResults, &err);

	_analysisStatus								= Status::complete;
	_analysisResults["results"]["inputOptions"]	= _imageOptions;

	sendAnalysisResults();

	_analysisStatus								= Status::empty;
	_engineState								= engineState::idle;
}

void Engine::editImage()
{
	std::string optionsJson	= _imageOptions.toStyledString(),
				result		= jaspRCPP_editImage(_analysisName.c_str(), optionsJson.c_str(), _analysisId);

	// JSONCPP_STRING          err;
	// Json::CharReaderBuilder jsonReaderBuilder;
	// std::unique_ptr<Json::CharReader> const jsonReader(jsonReaderBuilder.newCharReader());

	// jsonReader->parse(result.c_str(), result.c_str() + result.length(), &_analysisResults, &err);

	Json::Reader().parse(result, _analysisResults, false);

	if(_analysisResults.isMember("results"))
		_analysisResults["results"]["request"] = _imageOptions.get("request", -1);

	_analysisStatus			= Status::complete;

	sendAnalysisResults();

	_analysisStatus			= Status::empty;
	_engineState			= engineState::idle;
}

void Engine::rewriteImages()
{
	jaspRCPP_rewriteImages(_analysisName.c_str(), _analysisId);

	/* Already sent from R! (Through jaspResultsCPP$send())
	_analysisStatus				= Status::complete;
	_analysisResults			= Json::Value();
	_analysisResults["status"]	= analysisResultStatusToString(analysisResultStatus::imagesRewritten);

	sendAnalysisResults();*/ 

	_analysisStatus				= Status::empty;
	_engineState				= engineState::idle;
}


analysisResultStatus Engine::getStatusToAnalysisStatus()
{
	switch (_analysisStatus)
	{
	case Status::running:
	case Status::changed:	return analysisResultStatus::running;
	case Status::complete:	return analysisResultStatus::complete;
	default:				return analysisResultStatus::fatalError;
	}
}


void Engine::sendAnalysisResults()
{
	Json::Value response			= Json::Value(Json::objectValue);

	response["typeRequest"]			= engineStateToString(engineState::analysis);
	response["id"]					= _analysisId;
	response["name"]				= _analysisName;
	response["revision"]			= _analysisRevision;
	response["progress"]			= Json::nullValue;
	
	bool					sensibleResultsStatus	= _analysisResults.isObject() && _analysisResults.get("status", Json::nullValue) != Json::nullValue;
	analysisResultStatus	resultStatus			= !sensibleResultsStatus ? getStatusToAnalysisStatus() : analysisResultStatusFromString(_analysisResults["status"].asString());

	response["results"] = _analysisResults.get("results", _analysisResults);
	response["status"]  = analysisResultStatusToString(resultStatus);

	sendString(response);
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
		filesToKeep.push_back(filesToKeepValue.asString());

	stringvec tempFilesFromLastTime = TempFiles::retrieveList(_analysisId);

	Utils::remove(tempFilesFromLastTime, filesToKeep);

	TempFiles::deleteList(tempFilesFromLastTime);
}

void Engine::stopEngine()
{
	Log::log() << "Engine::stopEngine() received, closing engine." << std::endl;

	switch(_engineState)
	{
	default:							/* everything not mentioned is fine */	break;
	case engineState::analysis:			_analysisStatus = Status::aborted;		break;
	case engineState::filter:
	case engineState::filterByName:
	case engineState::computeColumn:	throw std::runtime_error("Unexpected data synch during " + engineStateToString(_engineState) + " somehow, you should not expect to see this exception ever.");
	};

	_engineState = engineState::stopped;

	JASPTIMER_PRINTALL();

	freeRBridgeColumns();
	sendEngineStopped();
}

void Engine::sendEngineStopped()
{
	_engineState					= engineState::stopped;
	Json::Value rCodeResponse		= Json::objectValue;
	rCodeResponse["typeRequest"]	= engineStateToString(_engineState);
	sendString(rCodeResponse);
}

void Engine::pauseEngine(const Json::Value & json)
{
	Log::log() << "Engine paused" << std::endl;

	switch(_engineState)
	{
	default:							/* everything not mentioned is fine */	break;
	case engineState::analysis:			_analysisStatus = Status::aborted;		break;
	case engineState::filter:
	case engineState::filterByName:
	case engineState::computeColumn:	throw std::runtime_error("Unexpected data synch during " + engineStateToString(_engineState) + " somehow, you should not expect to see this exception ever.");
	};

	_engineState = engineState::paused;

	freeRBridgeColumns();
	if(json.get("unloadData", false).asBool())
	{
		delete _dataSet;
		_dataSet = nullptr;
	}

	sendEnginePaused();
}

void Engine::receiveReloadData()
{
	Log::log() << "Engine::receiveReloadData()" << std::endl;

	//Im doing the following switch as a copy from Engine::pauseEngine, but probably the engine is always going to be idle anyway.
	switch(_engineState)
	{
	default:							/* everything not mentioned is fine */	break;
	case engineState::analysis:			_analysisStatus = Status::aborted;		break;
	case engineState::filter:
	case engineState::filterByName:
	case engineState::computeColumn:	throw std::runtime_error("Unexpected data synch during " + engineStateToString(_engineState) + " somehow, you should not expect to see this exception ever.");
	};

	//First send state, then load data
	sendEngineLoadingData();
	provideAndUpdateDataSet(); //Also triggers loading from DB and calls _datasetProvidedCallback
	reloadColumnNames();
	sendEngineResumed();
}

void Engine::sendEnginePaused()
{
	Json::Value rCodeResponse		= Json::objectValue;
	rCodeResponse["typeRequest"]	= engineStateToString(engineState::paused);

	sendString(rCodeResponse);
}

void Engine::resumeEngine(const Json::Value & jsonRequest)
{
	Log::log() << "Engine resuming and absorbing settings from request." << std::endl;

	absorbSettings(jsonRequest);

	_engineState = engineState::idle;
	sendEngineResumed();
}

void Engine::sendEngineResumed(bool justReloadedData)
{
	Log::log() << "Engine::sendEngineResumed()" << std::endl;
	
	Json::Value response			= Json::objectValue;
	response["typeRequest"]			= engineStateToString(engineState::resuming);
	response["justReloadedData"]	= justReloadedData;

	sendString(response);
}

void Engine::sendEngineLoadingData()
{
	Log::log() << "Engine::sendEngineLoadingData()" << std::endl;
	
	_engineState			= engineState::reloadData;
	Json::Value response	= Json::objectValue;
	response["typeRequest"]	= engineStateToString(_engineState);

	sendString(response);
}

void Engine::receiveLogCfg(const Json::Value & jsonRequest)
{
	Log::log() << "Log Config received" << std::endl;

	Log::parseLogCfgMsg(jsonRequest);

	//Show the buildoutput where and when it matters!
	jaspRCPP_runScript(("options(renv.config.install.verbose=" + std::string(Log::toCout() ? "TRUE" : "FALSE") + ")").c_str());

	Json::Value logCfgResponse		= Json::objectValue;
	logCfgResponse["typeRequest"]	= engineStateToString(engineState::logCfg);

	sendString(logCfgResponse);

	_engineState = engineState::idle;
}

void Engine::absorbSettings(const Json::Value & jsonRequest)
{
	_ppi				= jsonRequest.get("ppi",				_ppi				).asInt();
	_developerMode		= jsonRequest.get("developerMode",		_developerMode		).asBool();
	_imageBackground	= jsonRequest.get("imageBackground",	_imageBackground	).asString();
	_langR				= jsonRequest.get("languageCode",		_langR				).asString();
	_qLocaleName		= jsonRequest.get("localeQt",			_qLocaleName		).asString();
	_useThousandSeps	= jsonRequest.get("use1000Seps",		_useThousandSeps	).asBool();
	_numDecimals		= jsonRequest.get("numDecimals",		_numDecimals		).asInt();
	_fixedDecimals		= jsonRequest.get("fixedDecimals",		_fixedDecimals		).asBool();
	_exactPValues		= jsonRequest.get("exactPValues",		_exactPValues		).asBool();
	_normalizedNotation	= jsonRequest.get("normalizedNotation",	_normalizedNotation	).asBool();
	_resultFont			= jsonRequest.get("resultFont",			_resultFont			).asString();

	const char	* PAT	= std::getenv("GITHUB_PAT");
	
#ifdef _WIN32
	_putenv_s	("GITHUB_PAT",  jsonRequest.get("GITHUB_PAT",			PAT ? PAT : "").asCString());
#else
	setenv		("GITHUB_PAT",  jsonRequest.get("GITHUB_PAT",			PAT ? PAT : "").asCString(), 1);
#endif
	rbridge_setLANG(_langR);
	jaspRCPP_setDecimalSettings(_numDecimals, _fixedDecimals, _normalizedNotation, _exactPValues);
	jaspRCPP_setFontAndPlotSettings(_resultFont.c_str(), _ppi, _imageBackground.c_str());
	
	QColumnUtils::setCallbacksAndDefaultLocale(QLocale(tq(_qLocaleName)), _useThousandSeps);
}


void Engine::receiveSettings(const Json::Value & jsonRequest)
{
	Log::log() << "Settings received" << std::endl;

	absorbSettings(jsonRequest);

	Json::Value response	= Json::objectValue;
	response["typeRequest"]	= engineStateToString(engineState::settings);

	sendString(response);

	_engineState = engineState::idle;
}
