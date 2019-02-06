#include "jaspState.h"
#include "jaspResults.h"

Json::Value jaspState::convertToJSON()
{
	Json::Value obj			= jaspObject::convertToJSON();
	obj["environmentName"]	= _envName;

	return obj;
}

void jaspState::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);
	_envName = in.get("environmentName", _envName).asString();
}


void jaspState::setObject(Rcpp::RObject obj)
{
	if(!obj.isNULL())
		jaspResults::setObjectInEnv(_envName, obj);
}

Rcpp::RObject jaspState::getObject()
{
	if(jaspResults::objectExistsInEnv(_envName))
		return jaspResults::getObjectFromEnv(_envName);
	return NULL;
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
