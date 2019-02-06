#include "jaspModuleRegistration.h"
#include <fstream>
#include <cmath>

sendFuncDef			jaspResults::ipccSendFunc		= nullptr;
pollMessagesFuncDef jaspResults::ipccPollFunc		= nullptr;
std::string			jaspResults::_saveResultsHere	= "";
std::string			jaspResults::_baseCitation		= "";
Rcpp::Environment*	jaspResults::_RStorageEnv			= nullptr;

const std::string jaspResults::analysisChangedErrorMessage = "Analysis changed and will be restarted!";

void jaspResults::setSendFunc(sendFuncDef sendFunc)
{
	ipccSendFunc = sendFunc;
}

void jaspResults::setPollMessagesFunc(pollMessagesFuncDef pollFunc)
{
	ipccPollFunc = pollFunc;
}

void jaspResults::setBaseCitation(std::string baseCitation)
{
	_baseCitation = baseCitation;
}

void jaspResults::setResponseData(int analysisID, int revision)
{
	response["id"]			= analysisID;
	response["revision"]	= revision;
	response["progress"]	= -1;
}

void jaspResults::setSaveLocation(const char * newSaveLocation)
{
	_saveResultsHere = newSaveLocation;
}

jaspResults::jaspResults(std::string title, Rcpp::RObject oldState)
	: jaspContainer(title, jaspObjectType::results)
{
	if(_RStorageEnv != nullptr)
		delete _RStorageEnv;
	_RStorageEnv = new Rcpp::Environment(Rcpp::Environment::global_env());

	if(!oldState.isNULL() && Rcpp::is<Rcpp::List>(oldState) && Rcpp::as<Rcpp::List>(oldState).containsElementNamed("figures"))
	{
		//Let's try to load all previous plots from the state!
		Rcpp::List figures = Rcpp::as<Rcpp::List>(oldState)["figures"];

		for(Rcpp::List plotInfo : figures)
			if(plotInfo.containsElementNamed("envName") && plotInfo.containsElementNamed("obj"))
			{
				std::string envName = Rcpp::as<std::string>(plotInfo["envName"]);
				(*_RStorageEnv)[envName] = plotInfo["obj"];
			}
	}

	setStatus("running");

	if(_baseCitation != "")
		addCitation(_baseCitation);

	if(_saveResultsHere != "")
		loadResults();
}

void jaspResults::setStatus(std::string status)
{
	response["status"] = status;
}

std::string jaspResults::getStatus()
{
	return response["status"].asString();
}

void jaspResults::complete()
{
	completeChildren();

	if(getStatus() == "running" || getStatus() == "waiting")
		setStatus("complete");

	send();
	saveResults();
}

void jaspResults::saveResults()
{
	JASP_OBJECT_TIMERBEGIN
	if(_saveResultsHere == "")
	{
		jaspPrint("Did not store jaspResults");
		return;
	}

	std::ofstream saveHere(_saveResultsHere);
	Json::Value json = convertToJSON();

	saveHere << json.toStyledString();
	JASP_OBJECT_TIMEREND(saveResults)
}

void jaspResults::loadResults()
{
	JASP_OBJECT_TIMERBEGIN
	_previousOptions = Json::nullValue;

	if(_saveResultsHere == "") return;

	std::ifstream loadThis(_saveResultsHere);

	if(!loadThis.is_open()) return;

	Json::Value val;

	Json::Reader().parse(loadThis, val);

	if(!val.isObject()) return;

	convertFromJSON_SetFields(val);

	JASP_OBJECT_TIMEREND(loadResults);
}

void jaspResults::changeOptions(std::string opts)
{
	_previousOptions = _currentOptions;

	setOptions(opts);
}

void jaspResults::setOptions(std::string opts)
{
	Json::Reader().parse(opts, _currentOptions);
	jaspObject::currentOptions = _currentOptions;

	if(_previousOptions != Json::nullValue)
		pruneInvalidatedData();
}

void jaspResults::pruneInvalidatedData()
{
	checkDependenciesChildren(_currentOptions);
}

void jaspResults::send(std::string otherMsg)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	JASPprint("send was called!");
#endif

	if(ipccSendFunc != nullptr)
		(*ipccSendFunc)(otherMsg == "" ? constructResultJson() : otherMsg.c_str());
}

void jaspResults::checkForAnalysisChanged()
{
	if(ipccPollFunc == nullptr)
		return;

	if((*ipccPollFunc)())
	{
		setStatus("changed");
		Rf_error(analysisChangedErrorMessage.c_str());
	}
}


void jaspResults::childrenUpdatedCallbackHandler()
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	JASPprint("One of jaspResults children was updated!\n");
#endif

	checkForAnalysisChanged(); //can "throw" Rf_error

	int curTime = getCurrentTimeMs();
	if(_sendingFeedbackLastTime == -1 || (curTime - _sendingFeedbackLastTime) > _sendingFeedbackInterval)
	{
		send();
		_sendingFeedbackLastTime = curTime;
	}
}

Json::Value jaspResults::response = Json::Value(Json::objectValue);

const char * jaspResults::constructResultJson()
{
	response["typeRequest"]	= "analysis"; // Should correspond to engineState::analysis to string
	response["results"]		= dataEntry();
	response["name"]		= response["results"]["title"];

	if(errorMessage != "")
	{
		response["results"]["error"]		= true;
		response["results"]["errorMessage"] = errorMessage;
	}

	static std::string msg;
	msg = response.toStyledString();

#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "Result JSON:\n" << msg << "\n\n" << std::flush;
#endif

	return msg.c_str();
}

Json::Value jaspResults::metaEntry()
{
	Json::Value meta(Json::arrayValue);

	std::vector<std::string> orderedDataFields = getSortedDataFields();

	for(std::string field: orderedDataFields)
		if(_data[field]->shouldBePartOfResultsJson())
			meta.append(_data[field]->metaEntry());

	return meta;
}

Json::Value jaspResults::dataEntry()
{
	Json::Value dataJson(jaspObject::dataEntry());

	dataJson["title"]	= _title;
	dataJson["name"]	= getUniqueNestedName();
	dataJson[".meta"]	= metaEntry();

	for(std::string field: getSortedDataFields())
		if(_data[field]->shouldBePartOfResultsJson())
			dataJson[_data[field]->getUniqueNestedName()] = _data[field]->dataEntry();

	return dataJson;
}



void jaspResults::setErrorMessage(std::string msg)
{
	if(msg.find(analysisChangedErrorMessage) != std::string::npos)
		return; //we do not wanna report analysis changed as an error I think

	errorMessage = msg;
	setStatus("error");
}

Rcpp::List jaspResults::getPlotObjectsForState()
{
	Rcpp::List returnThis;
	Rcpp::Shield<Rcpp::List> protectList(returnThis);

	JASP_OBJECT_TIMERBEGIN
	addSerializedPlotObjsForStateFromJaspObject(this, returnThis);
	JASP_OBJECT_TIMEREND(getting plot objects)
	return returnThis;
}

void jaspResults::addSerializedPlotObjsForStateFromJaspObject(jaspObject * obj, Rcpp::List & pngImgObj)
{
	if(obj->getType() == jaspObjectType::plot)
	{
		jaspPlot * plot = (jaspPlot*)obj;
		if(plot->_filePathPng != "")
		{
			Rcpp::List pngImg;
			pngImg["obj"]					= plot->getPlotObject();
			pngImg["width"]					= plot->_width;
			pngImg["height"]				= plot->_height;
			pngImg["envName"]				= plot->_envName;
			pngImgObj[plot->_filePathPng]	= pngImg;
		}
	}

	for(auto c : obj->getChildren())
		addSerializedPlotObjsForStateFromJaspObject(c, pngImgObj);
}

Rcpp::List jaspResults::getPlotPathsForKeep()
{
	Rcpp::List returnThis;
	auto * protectList  = new Rcpp::Shield<Rcpp::List>(returnThis);

	addPlotPathsForKeepFromJaspObject(this, returnThis);

	delete protectList;
	return returnThis;
}

void jaspResults::addPlotPathsForKeepFromJaspObject(jaspObject * obj, Rcpp::List & pngPlotPaths)
{
	if(obj->getType() == jaspObjectType::plot)
	{
		jaspPlot * plot = (jaspPlot*)obj;
		if(plot->_filePathPng != "")
			pngPlotPaths.push_back(plot->_filePathPng);
	}

	for(auto c : obj->getChildren())
		addPlotPathsForKeepFromJaspObject(c, pngPlotPaths);
}

Rcpp::List jaspResults::getKeepList()
{
	Rcpp::List keep = getPlotPathsForKeep();
	keep.push_front(std::string(_saveResultsHere));
	keep.push_front(_relativePathKeep);

	return keep;
}

Json::Value jaspResults::convertToJSON()
{
	Json::Value obj			= jaspContainer::convertToJSON();

	obj["relativePathKeep"] = _relativePathKeep;
	obj["options"]			= _currentOptions;

	return obj;
}

void jaspResults::convertFromJSON_SetFields(Json::Value in)
{
	jaspContainer::convertFromJSON_SetFields(in);

	_relativePathKeep	= in.get("relativePathKeep",	"null").asString();
	_currentOptions		= in.get("options",				Json::objectValue);
	_previousOptions	= _currentOptions;
}

void jaspResults::startProgressbar(int expectedTicks, int timeBetweenUpdatesInMs)
{
	_progressbarExpectedTicks		= expectedTicks;
	_progressbarBetweenUpdatesTime	= timeBetweenUpdatesInMs;
	_progressbarLastUpdateTime		= getCurrentTimeMs();
	_progressbarTicks				= 0;

	response["progress"]			= 0;

	send();
}

void jaspResults::progressbarTick()
{
	checkForAnalysisChanged();

	_progressbarTicks++;

	int progress			= std::lround(100.0f * ((float)_progressbarTicks) / ((float)_progressbarExpectedTicks));	progress				= std::min(100, std::max(progress, 0));
	response["progress"]	= progress;

	int curTime = getCurrentTimeMs();
	if(curTime - _progressbarLastUpdateTime > _progressbarBetweenUpdatesTime || progress == 100)
	{
		send();
		_progressbarLastUpdateTime = curTime;
	}
}

//implementation here in jaspResults.cpp to make sure we have access to all constructors
jaspObject * jaspObject::convertFromJSON(Json::Value in)
{
	jaspObjectType newType = jaspObjectTypeStringToObjectType(in.get("type", "").asString());

	jaspObject * newObject = nullptr;

	switch(newType)
	{
	case jaspObjectType::container:	newObject = new jaspContainer();	break;
	case jaspObjectType::table:		newObject = new jaspTable();		break;
	case jaspObjectType::plot:		newObject = new jaspPlot();			break;
	case jaspObjectType::json:		newObject = new jaspJson();			break;
	//case jaspObjectType::list:	newObject = new jaspList();			break;
	case jaspObjectType::html:		newObject = new jaspHtml();			break;
	case jaspObjectType::state:		newObject = new jaspState();		break;
	//case jaspObjectType::results:	newObject = new jaspResults();		break;
	default:						throw std::runtime_error("Cant understand this type");
	}

	if(newObject != nullptr) newObject->convertFromJSON_SetFields(in);

	return newObject;
}

Rcpp::RObject jaspResults::getObjectFromEnv(std::string envName)
{
	if(_RStorageEnv->exists(envName))
		return (*_RStorageEnv)[envName];
	return NULL;
}

void jaspResults::setObjectInEnv(std::string envName, Rcpp::RObject obj)
{
	(*_RStorageEnv)[envName] = obj;
}

bool jaspResults::objectExistsInEnv(std::string envName)
{
	return _RStorageEnv->exists(envName);
}
