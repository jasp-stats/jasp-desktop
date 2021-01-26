#include "jaspQmlSource.h"

jaspQmlSource::jaspQmlSource(const std::string & sourceID) : jaspTable(), _sourceID(sourceID)
{
	_type = jaspObjectType::qmlSource;
}

Json::Value jaspQmlSource::dataEntry(std::string & errorMessage) const
{
	Json::Value dataJson(jaspTable::dataEntry(errorMessage));

	dataJson["sourceID"] = _sourceID;

	return dataJson;
}

void jaspQmlSource::convertFromJSON_SetFields(Json::Value in)
{
	jaspTable::convertFromJSON_SetFields(in);

	_sourceID = in["sourceID"].asString();
}

Json::Value jaspQmlSource::convertToJSON() const
{
	Json::Value obj		= jaspTable::convertToJSON();
	obj["sourceID"]		= _sourceID;

	return obj;
}
