#include "jaspState.h"
#include "jaspResults.h"

Json::Value jaspState::convertToJSON()
{
	Json::Value obj		= jaspObject::convertToJSON();

	static Rcpp::Function serialize("serialize");
	Rcpp::Vector<RAWSXP> stateObjectSerialized = serialize(Rcpp::_["object"] = jaspResults::getObjectFromEnv(_envName), Rcpp::_["connection"] = R_NilValue, Rcpp::_["ascii"] = true);

	obj["environmentName"]	= _envName;
	obj["ObjectSerialized"] = std::string(stateObjectSerialized.begin(), stateObjectSerialized.end());

	return obj;
}

void jaspState::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_envName					= in.get("environmentName", _envName).asString();
	std::string jsonPlotObjStr	= in.get("ObjectSerialized", "").asString();

	jaspResults::setObjectInEnv(_envName, Rcpp::Vector<RAWSXP>(jsonPlotObjStr.begin(), jsonPlotObjStr.end()));
}


void jaspState::setObject(Rcpp::RObject obj)
{
	jaspResults::setObjectInEnv(_envName, obj);
}

Rcpp::RObject jaspState::getObject()
{
	return jaspResults::getObjectFromEnv(_envName);
}

std::string jaspState::dataToString(std::string prefix)
{
	std::stringstream out;

	out << prefix << "object stored: "	<< ( jaspResults::objectExistsInEnv(_envName) ? "no" : "yes") << "\n";

	return out.str();
}

void jaspState::initEnvName()
{
	static int counter = 0;

	_envName = "state_" + std::to_string(counter++);
}
