#include "jaspQmlSource.h"

jaspQmlSource::jaspQmlSource(const std::string & sourceID) : jaspJson(), _sourceID(sourceID)
{
	_type = jaspObjectType::qmlSource;
}

Json::Value jaspQmlSource::dataEntry(std::string & errorMessage) const
{
	Json::Value dataJson(jaspJson::dataEntry(errorMessage));

	dataJson["sourceID"] = _sourceID;

	return dataJson;
}

void jaspQmlSource::convertFromJSON_SetFields(Json::Value in)
{
	jaspJson::convertFromJSON_SetFields(in);

	_sourceID = in["sourceID"].asString();
}

Json::Value jaspQmlSource::convertToJSON() const
{
	Json::Value obj		= jaspJson::convertToJSON();
	obj["sourceID"]		= _sourceID;

	return obj;
}
