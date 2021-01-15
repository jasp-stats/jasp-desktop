#include "jaspModuleRegistration.h"
#include <fstream>
#include <cmath>
#include "boost/nowide/fstream.hpp"
#include "boost/nowide/cstdio.hpp"

typedef boost::nowide::ofstream bofstream; //Use this to work around problems on Windows with utf8 conversion
typedef boost::nowide::ifstream bifstream;

sendFuncDef			jaspResults::_ipccSendFunc		= nullptr;
pollMessagesFuncDef jaspResults::_ipccPollFunc		= nullptr;
std::string			jaspResults::_saveResultsHere	= "";
std::string			jaspResults::_saveResultsRoot	= "";
std::string			jaspResults::_writeSealRoot		= "";
std::string			jaspResults::_writeSealRelative	= "";
std::string			jaspResults::_baseCitation		= "";
Rcpp::Environment*	jaspResults::_RStorageEnv		= nullptr;
bool				jaspResults::_insideJASP		= false;
jaspResults*		jaspResults::_jaspResults		= nullptr;

void jaspResults::setSendFunc(sendFuncDef sendFunc)
{
	_ipccSendFunc = sendFunc;
}

void jaspResults::setPollMessagesFunc(pollMessagesFuncDef pollFunc)
{
	_ipccPollFunc = pollFunc;
}

void jaspResults::setBaseCitation(std::string baseCitation)
{
	_baseCitation = baseCitation;
}

void jaspResults::setResponseData(int analysisID, int revision)
{
	_response["id"]			= analysisID;
	_response["revision"]	= revision;
	
	Json::Value progress;
	progress["value"]		= -1;
	progress["label"]		= "";
	_response["progress"]	= progress;
}

void jaspResults::setSaveLocation(const std::string & root, const std::string & relativePath)
{
	_saveResultsRoot	= root;
	_saveResultsHere	= relativePath;

	if(_saveResultsRoot.size() > 0 && _saveResultsRoot[_saveResultsRoot.size() - 1] != '/')
		_saveResultsRoot.push_back('/');
}

void jaspResults::setWriteSealLocation(const std::string & root, const std::string & relativePath)
{
	_writeSealRoot		= root;
	_writeSealRelative	= relativePath;

	if(_writeSealRoot.size() > 0 && _writeSealRoot[_writeSealRoot.size() - 1] != '/')
		_writeSealRoot.push_back('/');
}

void jaspResults::setInsideJASP()
{
	_insideJASP = true;
}

jaspResults::jaspResults(std::string title, Rcpp::RObject oldState)
	: jaspContainer(title, jaspObjectType::results)
{
	_jaspResults = this;

	if(_RStorageEnv != nullptr)
		delete _RStorageEnv;

	if(_insideJASP)
	{
		Rcpp::Environment::global_env()["RStorageEnv"] = Rcpp::Environment::global_env().new_child(true);
		_RStorageEnv = new Rcpp::Environment(Rcpp::Environment::global_env()["RStorageEnv"]);

		if(_writeSealRoot + _writeSealRelative == "")
			throw std::runtime_error("Write seal location not given and we are running in JASP, this should never happen!");
	}
	else
		_RStorageEnv = new Rcpp::Environment(Rcpp::as<Rcpp::Environment>(Rcpp::Environment::namespace_env("jaspResults")[".plotStateStorage"]));

	bool imNotReincarnatedAfterBeingMurdered = lastWriteWorked();

	if(imNotReincarnatedAfterBeingMurdered && !oldState.isNULL() && Rcpp::is<Rcpp::List>(oldState))
		fillEnvironmentWithStateObjects(Rcpp::as<Rcpp::List>(oldState));

	setStatus("running");

	if(_baseCitation != "")
		addCitation(_baseCitation);

	if(imNotReincarnatedAfterBeingMurdered && _saveResultsHere != "")
		loadResults();
}

void jaspResults::setStatus(std::string status)
{
	_response["status"] = status;
}

std::string jaspResults::getStatus()
{
	return _response["status"].asString();
}

void jaspResults::prepareForWriting()
{
	//Remove the seal if it is there or not doesnt matter
	boost::nowide::remove((_writeSealRoot + _writeSealRelative).c_str());
}

void jaspResults::finishWriting()
{
	//Let us write a small file that tells us writing stuff went well ( https://github.com/jasp-stats/INTERNAL-jasp/issues/884 )
	bofstream sealMe((_writeSealRoot + _writeSealRelative).c_str(), std::ios_base::trunc);

	sealMe << "Writing state, plot and jaspResults.json seems to have been successful!\n" << std::flush;

	sealMe.close();

	jaspPrint("Created Write Seal for jaspResults at: '" + _writeSealRoot + _writeSealRelative + "' ");
}

bool jaspResults::lastWriteWorked() const
{
	//Let us write a small file that tells us writing stuff went well ( https://github.com/jasp-stats/INTERNAL-jasp/issues/884 )
	bifstream seal((_writeSealRoot + _writeSealRelative).c_str(), std::ios_base::in);

	if(!seal.is_open()) return false;

	//std::cout << "Opened Write Seal for jaspResults to check if the last write worked from: '" << (_writeSealRoot + _writeSealRelative) << "' worked!" << std::endl;

	std::stringstream wholeSeal;

	wholeSeal << seal.rdbuf();

	seal.close();

	return wholeSeal.str().size() > 0;
}



void jaspResults::complete()
{
	completeChildren();

	_oldResults = nullptr; //It will get destroyed in DestroyAllAllocatedObjects

	if(getStatus() == "running" || getStatus() == "waiting")
		setStatus("complete");

	send();
	saveResults();
	finishWriting();
}

void jaspResults::saveResults()
{
	JASP_OBJECT_TIMERBEGIN
	if(_saveResultsHere == "")
	{
		jaspPrint("Did not store jaspResults");
		return;
	}

	//std::cout << "Going to try to save jaspResults.json to '" << _saveResultsRoot << _saveResultsHere << "'" << std::endl;

	bofstream saveHere((_saveResultsRoot + _saveResultsHere).c_str(), std::ios_base::trunc);

	if(!saveHere.good())
	{
		static std::string error;
		error = "Could not open file for saving jaspResults! File: '" + _saveResultsRoot + _saveResultsHere + "'";
		Rf_error(error.c_str());;
	}

	Json::Value json = convertToJSON();

	Json::StyledWriter styledWriter;
	saveHere << styledWriter.write(json);

	saveHere.close();

	JASP_OBJECT_TIMEREND(saveResults)
}

void jaspResults::loadResults()
{
	JASP_OBJECT_TIMERBEGIN
	_previousOptions = Json::nullValue;

	if(_saveResultsHere == "") return;

	bifstream loadThis((_saveResultsRoot + _saveResultsHere).c_str());

	if(!loadThis.is_open()) return;

	Json::Value val;

	Json::Reader().parse(loadThis, val);

	loadThis.close();

	if(!val.isObject())
	{
		static std::string error;
		error = "loading jaspResults had a problem, '" + _saveResultsRoot + _saveResultsHere + "' wasn't a JSON object!";
		Rf_error(error.c_str());;
	}

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

void jaspResults::storeOldResults()
{
	_oldResults = new jaspContainer();
	_oldResults->convertFromJSON_SetFields(jaspContainer::convertToJSON());
	_oldResults->letChildrenRun();
}

void jaspResults::pruneInvalidatedData()
{
	storeOldResults();

	checkDependenciesChildren(_currentOptions);
}

void jaspResults::send(std::string otherMsg)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	JASPprint("send was called!");
#endif

	if(_ipccSendFunc != nullptr)
		(*_ipccSendFunc)(otherMsg == "" ? constructResultJson() : otherMsg.c_str());
}

void jaspResults::checkForAnalysisChanged()
{
	if(_ipccPollFunc == nullptr)
		return;

	if((*_ipccPollFunc)())
	{
		jaspPrint("Polling for analysis changes found a change, analysis should restart!");
		setStatus("changed");
		static Rcpp::Function signalAnalysisAbort = jaspResults::isInsideJASP() ? Rcpp::Function("signalAnalysisAbort") : Rcpp::Environment::namespace_env("jaspResults")["signalAnalysisAbort"];
		signalAnalysisAbort();
	}
}


void jaspResults::childrenUpdatedCallbackHandler(bool ignoreSendTimer)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	JASPprint("One of jaspResults children was updated!\n");
#endif

	checkForAnalysisChanged(); //can "throw" Rf_error

	if(!containsNonContainer())
		return;

	int curTime = getCurrentTimeMs();
	if(
		ignoreSendTimer													||
		_sendingFeedbackLastTime == -1									||
		(curTime - _sendingFeedbackLastTime) > _sendingFeedbackInterval
	)
	{
		send();
		_sendingFeedbackLastTime = curTime;
	}
}

Json::Value jaspResults::_response = Json::Value(Json::objectValue);

const char * jaspResults::constructResultJson()
{
	_response["typeRequest"]	= "analysis"; // Should correspond to engineState::analysis to string
	_response["results"]		= dataEntry();
	_response["name"]			= _response["results"]["title"];

	if(errorMessage != "" )
	{
		_response["results"]["error"]		= true;
		_response["results"]["errorMessage"] = errorMessage;
	}
	else if (_error)
	{
		_response["results"]["error"]		= true;
		_response["results"]["errorMessage"] = "Analyis returned an error but no errormessage...";
	}

	static std::string msg;
	msg = _response.toStyledString();

#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "Result JSON:\n" << msg << "\n\n" << std::flush;
#endif

	return msg.c_str();
}



Json::Value jaspResults::metaEntry() const
{
	Json::Value meta(Json::arrayValue);

	std::vector<std::string> orderedDataFields = getSortedDataFieldsWithOld(_oldResults);

	for(std::string field: orderedDataFields)
	{
		jaspObject *	obj			= getJaspObjectNewOrOld(field, _oldResults);
		bool			objIsOld	= jaspObjectComesFromOldResults(field, _oldResults);

		if(obj->shouldBePartOfResultsJson())
			meta.append(obj->metaEntry(objIsOld || !_oldResults ? nullptr : _oldResults->getJaspObjectFromData(field)));
	}

	return meta;
}

Json::Value jaspResults::dataEntry(std::string &) const
{
	Json::Value dataJson(jaspObject::dataEntryBase());

	dataJson["title"]	= _title;
	dataJson["name"]	= getUniqueNestedName();
	dataJson[".meta"]	= metaEntry();

	for(std::string field: getSortedDataFieldsWithOld(_oldResults))
	{
		jaspObject *	obj			= getJaspObjectNewOrOld(field, _oldResults);
		bool			objIsOld	= jaspObjectComesFromOldResults(field, _oldResults);
		std::string		dummyError	= "";

		if(obj->shouldBePartOfResultsJson())
			dataJson[obj->getUniqueNestedName()]	= obj->dataEntry(objIsOld || !_oldResults ? nullptr : _oldResults->getJaspObjectFromData(field), dummyError);
	}

	return dataJson;
}



void jaspResults::setErrorMessage(std::string msg, std::string errorStatus)
{
	errorMessage = msg;
	setStatus(errorStatus);
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
			pngImg["revision"]				= plot->_revision;
			pngImg["envName"]				= plot->_envName;
			pngImg["getUnique"]				= plot->getUniqueNestedName();
			pngImgObj[plot->_filePathPng]	= pngImg;
		}
	}

	for(auto c : obj->getChildren())
		addSerializedPlotObjsForStateFromJaspObject(c, pngImgObj);
}

Rcpp::List jaspResults::getOtherObjectsForState()
{
	Rcpp::List returnThis;
	Rcpp::Shield<Rcpp::List> protectList(returnThis);

	JASP_OBJECT_TIMERBEGIN
	addSerializedOtherObjsForStateFromJaspObject(this, returnThis);
	JASP_OBJECT_TIMEREND(getting other objects)
	return returnThis;
}

void jaspResults::addSerializedOtherObjsForStateFromJaspObject(jaspObject * obj, Rcpp::List & cumulativeList)
{
	if(obj->getType() == jaspObjectType::state)
	{
		jaspState * state				= (jaspState*)obj; //If other objects are needed this code can be generalized

		if(objectExistsInEnv(state->_envName))
			cumulativeList[state->_envName]	= state->getObject();
	}

	for(auto child : obj->getChildren())
		addSerializedOtherObjsForStateFromJaspObject(child, cumulativeList);
}

void jaspResults::fillEnvironmentWithStateObjects(Rcpp::List state)
{
	if(state.containsElementNamed("figures"))
	{
		//Let's try to load all previous plots from the state!
		Rcpp::List figures = state["figures"];

		for(Rcpp::List plotInfo : figures)
			if(plotInfo.containsElementNamed("envName") && plotInfo.containsElementNamed("obj"))
			{
				std::string envName = Rcpp::as<std::string>(plotInfo["envName"]);
				(*_RStorageEnv)[envName] = plotInfo;
			}
	}

	if(state.containsElementNamed("other"))
	{
		//Let's try to load all previous plots from the state!
		Rcpp::List others = state["other"];
		Rcpp::List names  = others.names();

		for(std::string name : names)
			(*_RStorageEnv)[name] = others[name];
	}
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
	keep.push_front(std::string(_writeSealRelative));
	keep.push_front(_relativePathKeep);

	return keep;
}

Json::Value jaspResults::convertToJSON() const
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



void jaspResults::startProgressbar(int expectedTicks, std::string label)
{
	_progressbarExpectedTicks		= expectedTicks;
	_progressbarLastUpdateTime		= getCurrentTimeMs();
	_progressbarTicks				= 0;

	Json::Value progress;
	progress["value"]		= 0;
	progress["label"]		= label;
	_response["progress"]	= progress;

	send();
}

void jaspResults::progressbarTick()
{
	checkForAnalysisChanged();

	_progressbarTicks++;

	int progressValue				= int(std::lround(100.0f * (float(_progressbarTicks) / float(_progressbarExpectedTicks))));
	progressValue					= std::min(100, std::max(progressValue, 0));
	_response["progress"]["value"]	= progressValue;

	int curTime = getCurrentTimeMs();
	if(curTime - _progressbarLastUpdateTime > _progressbarBetweenUpdatesTime || progressValue == 100)
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
	case jaspObjectType::qmlSource:	newObject = new jaspQmlSource();	break;
	case jaspObjectType::container:	newObject = new jaspContainer();	break;
	case jaspObjectType::table:		newObject = new jaspTable();		break;
	case jaspObjectType::plot:		newObject = new jaspPlot();			break;
	case jaspObjectType::json:		newObject = new jaspJson();			break;
	//case jaspObjectType::list:	newObject = new jaspList();			break;
	case jaspObjectType::html:		newObject = new jaspHtml();			break;
	case jaspObjectType::state:		newObject = new jaspState();		break;
	case jaspObjectType::column:	newObject = new jaspColumn();		break;
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
	return R_NilValue;
}

void jaspResults::setObjectInEnv(std::string envName, Rcpp::RObject obj)
{
	(*_RStorageEnv)[envName] = obj;
}

bool jaspResults::objectExistsInEnv(std::string envName)
{
	return _RStorageEnv->exists(envName);
}
