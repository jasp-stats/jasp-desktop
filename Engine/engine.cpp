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
#include "qutils.h"
#include "databaseinterface.h"
#include "r_functionwhitelist.h"
#include <algorithm>

//Installed once per engine session, together with options(JASP.find.package.map), right before a
//module loads (Engine::receiveModuleRequestMessage). Stock find.package() collects EVERY match:
//it runs one vectorized file.exists() sweep across all of .libPaths() per lookup (also through
//system.file), because it must gather all candidate dirs before prepending the loaded namespace's
//path and version-checking each. With the ~300-400 micro-library libpaths of a module that costs
//~20ms per call on Windows/NTFS (measured, R 4.5.2), paid per namespace load and on every
//system.file() call. The map lets mapped packages skip the sweep entirely. Semantics are exactly
//preserved: for a mapped package we return the loaded namespace's path when loaded (as stock
//does), else the first hit in .libPaths() order (the map is built in that order); everything
//unmapped (base packages included) falls through to the untouched original. unlockBinding is
//legitimate here because JASP ships its own R; if a future R forbids it the tryCatch leaves the
//stock function in place - slow, but correct. See Docs/development/windows-binary-pkgs-libpaths.md.
static const char * pkgMapFindPackagePatch = R"RCODE(
(function()
{
	install <- function()
	{
		if (isTRUE(getOption('JASP.find.package.map.installed')))
			return(invisible(TRUE))

		ok <- tryCatch({
			orig <- base::find.package
			newfp <- function(package = NULL, lib.loc = NULL, quiet = FALSE, verbose = getOption('verbose')) {
				map <- getOption('JASP.find.package.map') #read at call time so module switches can refresh it
				#is.null: the default search through .libPaths(); identical after unique(): loadNamespace's
				#import recursion passes c(lib.loc, .libPaths()), duplicating the vector at every depth —
				#unique() gives it back in original order, and duplicates never change a first-hit search
				if ((is.null(lib.loc) || identical(unique(lib.loc), .libPaths())) && !is.null(map) && length(package) == 1L && !is.na(package) && package %in% names(map)) {
					if (isNamespaceLoaded(package))
						return(if (identical(package, 'base')) system.file() else .getNamespaceInfo(asNamespace(package), 'path'))
					p <- file.path(map[[package]], package)
					if (file.exists(file.path(p, 'DESCRIPTION')))
						return(p)
				}
				orig(package, lib.loc, quiet, verbose)
			}
			ns <- asNamespace('base')
			unlockBinding('find.package', ns)
			assign('find.package', newfp, envir = ns)
			lockBinding('find.package', ns)
			TRUE
		}, error = function(e) FALSE)

			options(JASP.find.package.map.installed = ok)
		if (ok)
			cat('JASP find.package map active (', length(getOption('JASP.find.package.map')), ' entries)\n')
		else
			cat('JASP find.package map could not be installed, falling back to stock (slow) lookups\n') #cat, not warning: the eval wrapper suppresses warnings
		invisible(ok)
	}

	installLoadNamespaceNormalizer <- function()
	{
		if (isTRUE(getOption('JASP.loadNamespace.normalized')))
			return(invisible(TRUE))
		ok <- tryCatch({
			origln <- base::loadNamespace
			newln <- function(package, lib.loc = NULL, ...)
			{
				#loadNamespace threads c(lib.loc, .libPaths()) through its recursion, so the shelf
				#list duplicates at every dependency depth (285, 570, 855... entries). Duplicates are
				#meaningless in a search path (first occurrence decides everything, order preserved),
				#so dedup keeps the vectors bounded at length(.libPaths()) instead of growing with
				#chain depth — and stops deep fall-through sweeps from being amplified several-fold.
				if (is.character(lib.loc) && anyDuplicated(lib.loc))
					lib.loc <- unique(lib.loc)
				origln(package, lib.loc, ...)
			}
			ns <- asNamespace('base')
			unlockBinding('loadNamespace', ns)
			assign('loadNamespace', newln, envir = ns)
			lockBinding('loadNamespace', ns)
			TRUE
		}, error = function(e) FALSE)
		options(JASP.loadNamespace.normalized = ok)
		if (!ok)
			cat('JASP loadNamespace normalizer could not be installed; lib.loc vectors may duplicate\n')
		invisible(ok)
	}

	install()
	installLoadNamespaceNormalizer()
})()
)RCODE";

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
		
		sendEngineLoadingData();
		provideAndUpdateDataSet();
		_engineState = engineState::idle;
		sendEngineResumed(true);
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
	static int64_t lastCheckTime = -1;

	if(newlyIdle)
	{
		idleStartTime = Utils::currentSeconds();
		lastCheckTime = idleStartTime;
	}
	else
	{
		if(idleStartTime != -1 && idleStartTime + 10 < Utils::currentSeconds())
		{
			Log::log() << "Attempting to clean up memory used by engine/R a bit." << std::endl;
			rbridge_memoryCleaning();
			Log::log() << "Memory is cleaned up" << std::endl;
			idleStartTime = -1;
		}

		if(lastCheckTime != -1 && lastCheckTime + 15 < Utils::currentSeconds() && _engineState == engineState::idle)
		{
			//Re-entrancy guard: only do the background reload while the engine is truly idle. beIdle()
			//is normally only reached from the idle case, but this defends against any future caller
			//invoking it while a request is mid-flight (loadingData/analysis/...), which would otherwise
			//reload the shared DB underneath an in-progress, desktop-orchestrated operation.
			_engineState = engineState::loadingData;
			sendEngineLoadingData();
			
			float lastSent = 0.0f;
			provideAndUpdateDataSet(-1, [&](float p){
				if(p - lastSent > 0.01f || p >= 1.0f)
				{
					sendLoadingDataProgress(p);
					lastSent = p;
				}
			});
			
			_engineState = engineState::idle;
			sendEngineResumed(true); //A background reload did push loading-progress to ~1.0; signal it so
			                          //the desktop resets its loading bar (justReloadedData=true) instead of staying stuck.
			lastCheckTime = Utils::currentSeconds();
		}
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
			case engineState::computeDataSet:		receiveComputeDataSetMessage(jsonRequest);	break;
			case engineState::pauseRequested:		pauseEngine(jsonRequest);					break;
			case engineState::resuming:				resumeEngine(jsonRequest);					break;
			case engineState::moduleUninstallRequest:
			case engineState::moduleInstallRequest:
			case engineState::moduleLoadRequest:	receiveModuleRequestMessage(jsonRequest);	break;
			case engineState::stopRequested:		stopEngine();								break;
			case engineState::logCfg:				receiveLogCfg(jsonRequest);					break;
			case engineState::settings:				receiveSettings(jsonRequest);				break;
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
	int dataSetId				= jsonRequest.get("dataSetId", -1).asInt();

	runDefaultFilter(dataSetId, filter, generatedFilter, filterRequestId, referencedFilterIdsFromRequest(jsonRequest));
}

void Engine::receiveFilterByNameMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		Log::log() << "Unexpected filterByName message, current state is not idle (" << engineStateToString(_engineState) << ")";

	_engineState				= engineState::filter;
	std::string name			= jsonRequest.get("name",		"").asString();
	int dataSetId				= jsonRequest.get("dataSetId",	-1).asInt();
	int requestId				= jsonRequest.get("requestId",	-1).asInt();

	runFilterByName(name, dataSetId, requestId, referencedFilterIdsFromRequest(jsonRequest));
}

void Engine::runFilterByName(const std::string & name, int dataSetId, int requestId, const std::vector<int> & referencedFilterIds)
{
	provideAndUpdateDataSet(dataSetId);
	
	if(!_workspace || !_workspace->dataSetById(dataSetId) || !_workspace->dataSetById(dataSetId)->showFilter(name))
	{
		sendFilterByNameDone(name, dataSetId, "No workspace or filter in it found!", requestId);
		_engineState = engineState::idle;
		
		return;
	}
	
	Filter	*	localFilter			= _workspace->dataSetById(dataSetId)->shownFilter();
	std::string strippedFilter		= stringUtils::stripRComments(localFilter->rFilter());
	boolvec		filterResult;
	std::string RPossibleWarning;
	try
	{
		// the run's slice context is the filter being run - its own view encodes
		//the code (fixed for filter expressions), and the request's referencedFilterIds
		//sidecar drives the materialization of any referenced foreign slices.
		filterResult		= rbridge_applyFilter(strippedFilter, localFilter->generatedFilter(), localFilter, referencedFilterIds);
		RPossibleWarning	= jaspRCPP_getLastErrorMsg();
	}
	catch(filterException & e)
	{
		std::string error = std::string(e.what()).length() > 0 ? e.what() : "but it is unclear what the problem was...";
		error = "There was a problem running filter '" + name + "':\n" + error;
		Log::log() << error << std::endl;
		
		filterResult		= boolvec(localFilter && localFilter->data() ? localFilter->data()->rowCount() : 0, false); //in the case of a non-dataset filter a better default is probably better to set everything to false if something is wrong
		RPossibleWarning	= error;
	}
	


	DatabaseInterface::singleton()->transactionWriteBegin();
	localFilter->setFilterVector(filterResult);
	localFilter->setErrorMsg(RPossibleWarning);
	localFilter->incRevision();
	DatabaseInterface::singleton()->transactionWriteEnd();

	sendFilterByNameDone(name, dataSetId, RPossibleWarning, requestId);

	_engineState = engineState::idle;
}

void Engine::runDefaultFilter(int dataSetId, const std::string & filter, const std::string & generatedFilter, int filterRequestId, const std::vector<int> & referencedFilterIds)
{
	DataSet		*	dataSet				= provideAndUpdateDataSet(dataSetId);

	if(!dataSet)
	{
		sendFilterError(filterRequestId, "No DataSet found for dataSetId " + std::to_string(dataSetId) + ".");
		_engineState = engineState::idle;
		return;
	}
	
	try
	{		
		std::string		strippedFilter		= stringUtils::stripRComments(filter);
		// the run's slice context is the DEFAULT filter being run (its own view
		//encodes the code) plus the request's referencedFilterIds sidecar.
		boolvec			filterResult		= rbridge_applyFilter(strippedFilter, generatedFilter, dataSet->defaultFilter(), referencedFilterIds);
		std::string		RPossibleWarning	= jaspRCPP_getLastErrorMsg();

        Log::log() << "Engine::runDefaultFilter ran:\n\t" << strippedFilter << "\n\tRPossibleWarning='" << RPossibleWarning << "'\n\t\tfor revision " << dataSet->defaultFilter()->revision() << std::endl;

		dataSet->db().transactionWriteBegin();
		dataSet->defaultFilter()->setRFilter(filter);
		dataSet->defaultFilter()->setFilterVector(filterResult);
		dataSet->defaultFilter()->setErrorMsg(RPossibleWarning);
		dataSet->defaultFilter()->incRevision();
		dataSet->db().transactionWriteEnd();

		sendFilterResult(filterRequestId);
	}
	catch(filterException & e)
	{
		std::string error = std::string(e.what()).length() > 0 ? e.what() : "Something went wrong with the filter but it is unclear what.";
		
		while(dataSet->db().transactionReadDepth() > 0)
			dataSet->db().transactionReadEnd();
		
		dataSet->db().transactionWriteBegin();
		dataSet->defaultFilter()->setErrorMsg(error);
		dataSet->defaultFilter()->incRevision();
		dataSet->db().transactionWriteEnd();

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

void Engine::sendFilterByNameDone(const std::string & name, int dataSetId, const std::string & errorMessage, int requestId)
{
	Json::Value filterResponse(Json::objectValue);

	filterResponse["typeRequest"]	= engineStateToString(engineState::filterByName);
	filterResponse["name"]			= name;
	filterResponse["dataSetId"]		= dataSetId;
	filterResponse["errorMessage"]	= errorMessage;
	filterResponse["requestId"]		= requestId;

	sendString(filterResponse);
}

std::vector<int> Engine::referencedFilterIdsFromRequest(const Json::Value & jsonRequest)
{
	// the transient `referencedFilterIds` sidecar of a filter/compute
	//request - the desktop's scan of the run's code (the grammar helpers).
	//This extraction IS the strip point for these requests (mirroring the last-moment
	//doctrine): the ids are consumed for slice materialization only and are never forwarded
	//into anything R evaluates - the R-visible code is built by rbridge_prepareSliceRunScript
	//from the code text alone, and the sidecar member dies with the request json here.
	std::vector<int> ids;

	const Json::Value & sidecar = jsonRequest.get("referencedFilterIds", Json::Value::null);

	if(sidecar.isArray())
		for(const Json::Value & id : sidecar)
			if(id.isInt() || id.isUInt())
				ids.push_back(id.asInt());

	return ids;
}

void Engine::receiveRCodeMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		Log::log() << "Unexpected rCode message, current state is not idle (" << engineStateToString(_engineState) << ")";

				_engineState	= engineState::rCode;
	std::string rCode			= jsonRequest.get("rCode",			"").asString();
	int			rCodeRequestId	= jsonRequest.get("requestId",		-1).asInt(),
				dataSetId		= jsonRequest.get("dataSetId",		-1).asInt();
	bool		whiteListed		= jsonRequest.get("whiteListed",	true).asBool(),
				returnLog		= jsonRequest.get("returnLog",		false).asBool(),
				columnsByName	= jsonRequest.get("columnsByName",	false).asBool();

	if(returnLog)	runRCodeCommander(dataSetId, rCode, columnsByName);
	else			runRCode(dataSetId, rCode, rCodeRequestId, whiteListed);
}

// Evaluating arbitrary R code (as string) which returns a string
void Engine::runRCode(int dataSetId, const std::string & rCode, int rCodeRequestId, bool whiteListed)
{
	provideAndUpdateDataSet(dataSetId);
	

	std::string rCodeResult = whiteListed ? rbridge_evalRCodeWhiteListed(rCode.c_str(), true) : jaspRCPP_evalRCode(rCode.c_str(), true);
	bool		hadError	= rCodeResult == "null";

	if (hadError)	sendRCodeError(rCodeRequestId);
	else			sendRCodeResult(rCodeRequestId, rCodeResult);

	_engineState = engineState::idle;
}


void Engine::runRCodeCommander(int dataSetId, std::string rCode, bool columnsByName)
{
    DataSet * thereIsSomeDataDs = provideAndUpdateDataSet(dataSetId);
    bool thereIsSomeData = thereIsSomeDataDs && thereIsSomeDataDs->rowCount();


	static const std::string rCmdDataName = "data", rCmdFiltered = "filteredData";

	if(thereIsSomeData)
	{
		std::set<std::string> columnsMentioned;
		rCode = ColumnEncoder::columnEncoder()->encodeRScript(rCode, &columnsMentioned);

		//Only the columns the code mentions get loaded, also through whole string literals
		//(data[["LL"]]), and with the type asked for (data$LL.nominal). This selective read is only
		//done when the commander asks for "columns by name", otherwise the full datasets are read
		//below, so that positional access (data[, 3]) and nrow(data) keep working as they always did.
		const ColumnEncoder::colsPlusTypes wanted = ColumnEncoder::columnEncoder()->colsPlusTypesFor(columnsMentioned);

		if(!columnsByName)
		{
			//The old behaviour: always hand the commander the full datasets, with the types as shown.
			jaspRCPP_runScript((rCmdDataName	+ "<- .encodedFrame(.readFullDatasetToEnd());").c_str());
			jaspRCPP_runScript((rCmdFiltered	+ "<- .encodedFrame(.readFullFilteredDatasetToEnd());").c_str());
		}
		else
		{
			//The same requested read that analyses and filters use: datasetWanted says which columns
			//and types to load, and the frame override from jaspEncodedFrame.R resolves the string
			//literals at lookup time. There is no fallback to the full dataset here: code that names
			//no columns simply gets an empty frame (rows are kept, columns are not).
			rbridge_setWantedCols(wanted);

			jaspRCPP_runScript((rCmdDataName	+ "<- .encodedFrame(.readDataSetRequestedNativeUnfiltered());").c_str());
			jaspRCPP_runScript((rCmdFiltered	+ "<- .encodedFrame(.readDataSetRequestedNative());").c_str());
		}
	}

	std::string rCodeResult =	jaspRCPP_evalRCodeCommander(rCode.c_str());

	if(thereIsSomeData)
		rCodeResult = ColumnEncoder::decodeAll(rCodeResult);

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
	int			dataSetId		  =						 jsonRequest.get("dataSetId",	-1).asInt();

	runComputeColumn(dataSetId, computeColumnName, computeColumnCode, computeColumnType, referencedFilterIdsFromRequest(jsonRequest));
}

void Engine::receiveComputeDataSetMessage(const Json::Value & jsonRequest)
{
	if(_engineState != engineState::idle)
		Log::log() << "Unexpected compute dataset message, current state is not idle (" << engineStateToString(_engineState) << ")";

	_engineState = engineState::computeDataSet;

	std::string	computeCode				= jsonRequest.get("computeCode", "").asString();
	int			dataSetId				= jsonRequest.get("dataSetId", -1).asInt();
	int			defaultInputFilterId	= jsonRequest.get("defaultInputFilterId", -1).asInt();

	runComputeDataSet(dataSetId, computeCode, defaultInputFilterId, referencedFilterIdsFromRequest(jsonRequest));
}

void Engine::runComputeColumn(int dataSetId, const std::string & computeColumnName, const std::string & computeColumnCode, columnType computeColumnType, const std::vector<int> & referencedFilterIds)
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
	
    DataSet * compDataSet = provideAndUpdateDataSet(dataSetId);
    if(compDataSet)
	{
		try
		{
			Column * compCol = compDataSet->column(computeColumnName);
			
			const std::string useThisFilter = compCol ? compCol->computeFilter() : "";
			
			// The run's slice context is the COMPUTE
			//FILTER's view - the same view the data frame R reads inside
			//rbridge_evalRComputedColumn, and the namespace bare names in the code fall back
			//to. The output-name encode uses that same explicit handle; the process-global
			//current encoder is no longer consulted here. ""/unknown falls back to the default
			//filter - the same fallback showFilter applies.
			Filter * computeFilter = compDataSet->filter(useThisFilter.empty() ? DEFAULT_FILTER_NAME : useThisFilter);
			ColumnEncoder * runView = computeFilter ? computeFilter->columnEncoder() : nullptr;
			std::string computeColumnNameEnc = runView ? runView->encode(computeColumnName) : ColumnEncoder::columnEncoder()->encode(computeColumnName);
			
			std::string computeColumnResultStr		= rbridge_evalRComputedColumn(
							computeColumnCode, 
							"toString("+ setColumnFunction.at(computeColumnType) + "('" + computeColumnNameEnc +"', .calcedVals, 1))",
							useThisFilter,
							computeFilter,
							referencedFilterIds);
	
			computeColumnResponse["result"]			= computeColumnResultStr;
			computeColumnResponse["error"]			= jaspRCPP_getLastErrorMsg();
		}
		catch(std::exception & e)
		{
			computeColumnResponse["result"]			= "fail";
			computeColumnResponse["error"]			= e.what();
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

void Engine::runComputeDataSet(int dataSetId, const std::string & computeCode, int defaultInputFilterId, const std::vector<int> & referencedFilterIds)
{
	Log::log() << "Engine::runComputeDataSet()" << std::endl;

	Json::Value computeDataSetResponse		= Json::objectValue;
	computeDataSetResponse["typeRequest"]	= engineStateToString(engineState::computeDataSet);

	//The "shown" dataset during a computed-dataset run is the input that the R code reads from;
	//the output (the computed dataset itself) is written to by name via .setDataSet.
	//A computed dataset that evaluates R code must read from an *input* filter; without one
	//(-1) it must not silently compute from its own (previous) output as though it were its input.
	if(defaultInputFilterId == -1)
	{
		computeDataSetResponse["result"]	= "fail";
		computeDataSetResponse["error"]		= "This computed dataset has no input filter selected; choose an input filter before computing it.";
		sendString(computeDataSetResponse);
		_engineState = engineState::idle;
		return;
	}

	Filter * inputFilter = _workspace ? _workspace->filterById(defaultInputFilterId) : nullptr;
	if(!inputFilter)
	{
		computeDataSetResponse["result"]	= "fail";
		computeDataSetResponse["error"]		= "The input filter (id " + std::to_string(defaultInputFilterId) + ") could not be found in the engine's workspace.";
		sendString(computeDataSetResponse);
		_engineState = engineState::idle;
		return;
	}

	int inputId = inputFilter->data()->id();

	if(provideAndUpdateDataSet(inputId))
	{
		try
		{
			DataSet	*	output		= _workspace->dataSetById(dataSetId);
			if(!output)
				throw std::runtime_error("Computed dataset (id " + std::to_string(dataSetId) + ") not found in engine's workspace; aborting compute. The output dataset must be synced before it can be filled.");
			std::string	outputName	= output->name().toStdString();

			std::string computeDataSetResult = rbridge_evalRComputedDataSet(computeCode, outputName, inputFilter->name(), inputFilter, referencedFilterIds);

			computeDataSetResponse["result"]	= computeDataSetResult;
			computeDataSetResponse["error"]		= jaspRCPP_getLastErrorMsg();
		}
		catch(std::exception & e)
		{
			computeDataSetResponse["result"]	= "fail";
			computeDataSetResponse["error"]		= e.what();
		}
	}
	else
	{
		computeDataSetResponse["result"]		= "fail";
		computeDataSetResponse["error"]		= "No DataSet loaded in engine!";
	}

	sendString(computeDataSetResponse);

	_engineState = engineState::idle;
}

void Engine::receiveModuleRequestMessage(const Json::Value & jsonRequest)
{
	_engineState					= engineStateFromString(jsonRequest.get("typeRequest", Json::nullValue).asString());

	std::string		moduleRequest	= jsonRequest["moduleRequest"].asString();
	std::string		moduleCode		= jsonRequest["moduleCode"].asString();
	std::string		moduleName		= jsonRequest["moduleName"].asString();
	std::string		moduleLibPaths  = jsonRequest["moduleLibPaths"].asString();
	std::string		modulePkgMap	= jsonRequest.get("modulePkgMap", Json::nullValue).asString();

	Log::log() << "About to run module request for module '" << moduleName << "' and code to run:\n'" << moduleCode << "'" << std::endl;

	if(moduleStatusFromString((moduleRequest)) == moduleStatus::loading) {
		//Some jaspModules use jaspBase calls in their .onload so we first we need to prepare jaspbase
		//The pkg map (Windows micro-library layout) must be in place before anything loads, so that
		//even jaspBase's own resolution skips the libpaths sweep.
		std::string	setupCode = ".libPaths( " + moduleLibPaths + " ); ";
		if(!modulePkgMap.empty() && modulePkgMap != "character(0)")
			setupCode += "options(JASP.find.package.map = " + modulePkgMap + "); " + std::string(pkgMapFindPackagePatch) + " ";
		jaspRCPP_evalRCode((setupCode + "'success'").c_str(), false);
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
		_analysisFilter			= jsonRequest.get("filter",				"DEFAULT_FILTER").asString();
		_analysisDataSetId		= jsonRequest.get("dataSetId",			-1).asInt();
		_analysisDataKey		= jsonRequest.get("dataKey",			Json::nullValue).toStyledString();
		_analysisResultsMeta	= jsonRequest.get("resultsMeta",		Json::nullValue).toStyledString();
		_analysisStateKey		= jsonRequest.get("stateKey",			Json::nullValue).toStyledString();
		_analysisRevision		= jsonRequest.get("revision",			-1).asInt();
		_imageOptions			= jsonRequest.get("image",				Json::nullValue);
		_analysisRFile			= jsonRequest.get("rfile",				"").asString();
		_dynamicModuleCall		= jsonRequest.get("dynamicModuleCall",	"").asString();
		_resultFont				= jsonRequest.get("resultFont",			"").asString();
		_analysisPreloadData	= jsonRequest.get("preloadData",		false).asBool();
		_analysisMultiDataSet	= jsonRequest.get("multiDataSetAware",	false).asBool();
		_engineState			= engineState::analysis;

		//Never let a queue from a previous run leak into this one:
		setMultiDataSetQueue({});

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
		//Every results payload goes through the one global strict decode table:
		//a block per live filter of every loaded dataset plus the extras block, membership-only,
		//unknown tokens staying raw. Aware and non-aware runs are not different here - a non-aware
		//analysis may still live in a workspace with other datasets, and over-sweeping is harmless.
		//safeHtml is just a parameter of the sweep (the old currentEncoder-flip variant is gone);
		//desktop renders HTML, so safeHtml = true.
		ColumnEncoder::decodeJson(message, /*replaceNames=*/true, /*safeHtml=*/true);

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

	// Drawing a plot decodes its column names, which needs the analysis' dataset to be the current encoder.
	// That is only set once the dataset is provided, and an engine may get an image request before
	// it ran any analysis (e.g. a resize right after loading a .jasp file).
	if(_analysisStatus == Status::saveImg || _analysisStatus == Status::editImg || _analysisStatus == Status::rewriteImgs)
		provideAndUpdateDataSet(_analysisDataSetId);

	switch(_analysisStatus)
	{
	case Status::saveImg:		 saveImage();						return;
	case Status::editImg:		 editImage();						return;
	case Status::rewriteImgs:	 rewriteImages();					return;

	case Status::empty:
	case Status::aborted:
		//Acknowledge the abort/no-op to the desktop: it waits ENGINE_KILLTIME for an
		//answer and kills+restarts the engine when none comes. This branch used to
		//return silently, so *every* abort degenerated into a kill+restart cycle
		//(engines endlessly loading data + resuming while analyses were never scheduled).
		sendAbortAck();
		return;

	default:	break;
	}

	DataSet * dataset = provideAndUpdateDataSet(_analysisDataSetId);
	Log::log() << "Analysis will be run now, it has DataSet id " << _analysisDataSetId << " and filter '" << _analysisFilter << "', it " << (dataset ? " found a loaded one" : " did not find one though..") << "." << std::endl;

	Json::Value encodedAnalysisOptions = _analysisOptions;
	
	updateOptionsAccordingToMeta(encodedAnalysisOptions);

	std::string multiDataSetJson;

	//Check multiAware before preload: an aware analysis always runs preload-style (the datasets are
	//handed over through the queued reads), and preloadData is forced true for aware entries anyway
	//(Modules::AnalysisEntry::preloadData), so the flag must not gate this branch.
	if(_analysisMultiDataSet)
	{
		//Encoding and slice queueing happen per dataset from the .meta provenance the controls
		//stamped; shared with the syntax bridge through DataBridge so the two cannot drift apart.
		MultiDataSetRunPlan plan = prepareMultiDataSetRun(encodedAnalysisOptions, _analysisDataSetId, _analysisFilter, _analysisTitle);

		if(DataSet * primary = plan.primaryDataSetId >= 0 ? resolveWorkspace()->dataSetById(plan.primaryDataSetId) : nullptr)
			dataset = primary;

		_analysisColsTypes = plan.primaryCols;
		multiDataSetJson	= plan.multiDataSetJson.toStyledString();
	}
	else
	{
		//Non-aware: the classic single (shown) dataset path, encoded against the current encoder.
		_analysisColsTypes = ColumnEncoder::encodeColumnNamesinOptions(encodedAnalysisOptions, _analysisPreloadData);
	}

	if(dataset && !_analysisFilter.empty() && dataset->filter(_analysisFilter))
		dataset->showFilter(_analysisFilter); //Only show a filter that actually exists: showFilter(name)
		                                        //creates a new filter for an unknown name, which would
		                                        //silently fabricate one from a stale analysis request.

	// strip the transient `.filterIds` provenance from the ENCODED copy at the
	//very last moment before R sees it (prepareMultiDataSetRun consumed it above; the unencoded
	//_analysisOptions keeps the sidecar so a Status::changed re-run can consume it again).
	stripTransientProvenanceForR(encodedAnalysisOptions);

	_analysisResultsString = rbridge_runModuleCall(_analysisName, _analysisTitle, _dynamicModuleCall, _analysisDataKey,
								encodedAnalysisOptions.toStyledString(), _analysisStateKey, _analysisId, _analysisRevision, 
								_developerMode, _analysisColsTypes, _analysisPreloadData, multiDataSetJson);

	switch(_analysisStatus)
	{
	case Status::aborted:
		//The analysis was aborted while running (the desktop re-sends the request with
		//perform=abort). Acknowledge it: without this reply the desktop always fell
		//through to its ENGINE_KILLTIME kill+restart, making aborts useless.
		sendAbortAck();
		return;

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
	{
		_analysisResults["results"]["request"] = _imageOptions.get("request", -1);
		// When pngFile is present (jaspBase ≥ <next>), the true PNG path is
		// provided by R and the hack is unnecessary. Older jaspBase builds won't
		// have pngFile, so fall back to injecting the plot name as data so the
		// C++/JS pipeline can still identify which image to update.
		if (_analysisResults["results"].isMember("name")
			&& !_analysisResults["results"].isMember("data")
			&& !_analysisResults["results"].isMember("pngFile"))
			_analysisResults["results"]["data"] = _analysisResults["results"]["name"];
	}

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


void Engine::sendAbortAck()
{
	//Tell the desktop the abort was processed. analysisResultStatus::aborted lets
	//EngineRepresentation::processAnalysisReply clear its in-progress bookkeeping
	//and go back to scheduling instead of killing this engine.
	_analysisResults					= Json::Value(Json::objectValue);
	_analysisResults["status"]			= "aborted";
	_analysisResults["results"]			= Json::Value(Json::objectValue);

	sendAnalysisResults();

	_analysisStatus						= Status::empty;
	_engineState						= engineState::idle;
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
	case engineState::computeColumn:
	case engineState::computeDataSet:	throw std::runtime_error("Unexpected data synch during " + engineStateToString(_engineState) + " somehow, you should not expect to see this exception ever.");
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
	case engineState::computeColumn:
	case engineState::computeDataSet:	throw std::runtime_error("Unexpected data synch during " + engineStateToString(_engineState) + " somehow, you should not expect to see this exception ever.");
	};

	_engineState = engineState::paused;

	freeRBridgeColumns();
	if(json.get("unloadData", false).asBool())
	{
		rbridge_clearDataSet(); //Don't leave rbridge_dataSet pointing into the workspace we are about to free.
		delete _workspace;
		_workspace = nullptr;
	}

	sendEnginePaused();
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
	
	_engineState			= engineState::loadingData;
	Json::Value response	= Json::objectValue;
	response["typeRequest"]	= engineStateToString(_engineState);
	response["progress"]	= 0.0;

	sendString(response);
}

void Engine::sendLoadingDataProgress(float progress)
{
	Json::Value response	= Json::objectValue;
	response["typeRequest"]	= engineStateToString(engineState::loadingData);
	response["progress"]	= progress;

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
