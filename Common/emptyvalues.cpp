#include "emptyvalues.h"

EmptyValues::EmptyValues()
{

}

Json::Value EmptyValues::toJson() const
{
	Json::Value emptyValuesMap	= Json::objectValue;

	for (const auto & it : _map)
	{
		std::string colName		= it.first;
		auto		map			= it.second;
		Json::Value mapJson		= Json::objectValue;

		for (const auto & it2 : map)
			mapJson[std::to_string(it2.first)] = it2.second;

		emptyValuesMap[colName] = mapJson;
	}

	return emptyValuesMap;
}

void EmptyValues::fromJson(const Json::Value & emptyValuesMapJson)
{
	resetEmptyValues();

	if (!emptyValuesMapJson.isNull())
	{
		for (Json::Value::const_iterator iter = emptyValuesMapJson.begin(); iter != emptyValuesMapJson.end(); ++iter)
		{
			std::string colName	= iter.key().asString();
			Json::Value mapJson	= *iter;
			std::map<int, std::string> map;

			for (Json::Value::const_iterator iter2 = mapJson.begin(); iter2 != mapJson.end(); ++iter2)
			{
				int row					= stoi(iter2.key().asString());
				Json::Value valueJson	= *iter2;
				std::string value		= valueJson.asString();
				map[row]				= value;
			}
			storeInEmptyValues(colName, map);
		}
	}
}
