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
