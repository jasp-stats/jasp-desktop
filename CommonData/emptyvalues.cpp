#include "emptyvalues.h"
#include "columnutils.h"
#include "log.h"

EmptyValues::EmptyValues()
{
}

Json::Value EmptyValues::toJson() const
{
	Json::Value missingDataPerColumn(Json::objectValue);
	Json::Value emptyValuesPerColumn(Json::objectValue);


	for (const auto & it : _missingData)
	{
		std::string colName		= it.first;
		auto		map			= it.second;
		Json::Value mapJson		= Json::objectValue;

		for (const auto & it2 : map)
			mapJson[std::to_string(it2.first)] = it2.second;

		missingDataPerColumn[colName] = mapJson;
	}

	for (const auto & it : _customEmptyValuesPerColumn)
	{
		std::string colName		= it.first;
		stringset	values		= it.second;
		Json::Value mapJson		= Json::objectValue;

		emptyValuesPerColumn[colName] = stringSetToJson(values);
	}

	Json::Value result(Json::objectValue);
	result["workspaceEmptyValues"]	= stringSetToJson(_workspaceEmptyValues);
	result["emptyValuesPerColumn"]	= emptyValuesPerColumn;
	result["missingDataPerColumn"]	= missingDataPerColumn;

	return result;
}

void EmptyValues::fromJson(const Json::Value & emptyValuesMap)
{
	if (emptyValuesMap.isNull()) return;

	_missingData.clear();
	// For backward compatibility, check whether "missingData" is a member. If not, missingDataPerColumn is emptyValuesMapJson
	const Json::Value& missingDataPerColumn = emptyValuesMap.isMember("missingDataPerColumn") ? emptyValuesMap["missingDataPerColumn"] : emptyValuesMap;
	for (Json::Value::const_iterator iter = missingDataPerColumn.begin(); iter != missingDataPerColumn.end(); ++iter)
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
		_missingData[colName] = map;
	}

	if (emptyValuesMap.isMember("workspaceEmptyValues"))
	{
		_workspaceEmptyValues = jsonToStringSet(emptyValuesMap["workspaceEmptyValues"]);
		_workspaceDoubleEmptyValues = getDoubleValues(_workspaceEmptyValues);
	}

	_customEmptyValuesPerColumn.clear();
	_customDoubleEmptyValuesPerColumn.clear();
	if (emptyValuesMap.isMember("emptyValuesPerColumn"))
	{
		const Json::Value& emptyValuesPerColumn = emptyValuesMap["emptyValuesPerColumn"];
		if (emptyValuesPerColumn.isObject())
		{
			for (const std::string& colName : emptyValuesPerColumn.getMemberNames())
			{
				stringset stringValues = jsonToStringSet(emptyValuesPerColumn[colName]);
				_customEmptyValuesPerColumn[colName] = stringValues;
				_customDoubleEmptyValuesPerColumn[colName] = getDoubleValues(stringValues);
			}
		}
	}
}

const stringset& EmptyValues::workspaceEmptyValues() const
{
	return _workspaceEmptyValues;
}

void EmptyValues::setWorkspaceEmptyValues(const stringset& values)
{
	_workspaceEmptyValues = values;
	_workspaceDoubleEmptyValues = getDoubleValues(values);
}

const stringset& EmptyValues::emptyValues(const std::string& colName) const
{
	return _customEmptyValuesPerColumn.count(colName) ? _customEmptyValuesPerColumn.at(colName) : _workspaceEmptyValues;
}

const doubleset& EmptyValues::doubleEmptyValues(const std::string &colName) const
{
	return _customDoubleEmptyValuesPerColumn.count(colName) ? _customDoubleEmptyValuesPerColumn.at(colName) : _workspaceDoubleEmptyValues;
}

void EmptyValues::setCustomEmptyValues(const std::string &colName, const stringset& values)
{
	_customEmptyValuesPerColumn[colName] = values;
	_customDoubleEmptyValuesPerColumn[colName] = getDoubleValues(values);
}

const intstrmap& EmptyValues::missingData(const std::string &colName) const
{
	return _missingData.count(colName) ? _missingData.at(colName) : _emptyMissingData;
}

void EmptyValues::setMissingData(const std::string& colName, const intstrmap &data)
{
	_missingData[colName] = data;
}

void EmptyValues::resetEmptyValues()
{
	_missingData.clear();
	_workspaceEmptyValues.clear();
	_workspaceDoubleEmptyValues.clear();
	_customEmptyValuesPerColumn.clear();
	_customDoubleEmptyValuesPerColumn.clear();
}

Json::Value EmptyValues::stringSetToJson(const stringset &vec) const
{
	Json::Value result(Json::arrayValue);
	for (const std::string& str : vec)
		result.append(str);

	return result;
}

stringset EmptyValues::jsonToStringSet(const Json::Value &json) const
{
	stringset result;
	if (json.isArray())
	{
		for (const Json::Value& val : json)
			result.insert(val.asString());
	}

	return result;
}

doubleset EmptyValues::getDoubleValues(const stringset& values) const
{
	doubleset result;
	for (const std::string & val : values)
	{
		double doubleValue;
		if (ColumnUtils::getDoubleValue(val, doubleValue))
			result.insert(doubleValue);
	}

	return result;
}

void EmptyValues::setHasCustomEmptyValues(const std::string& colName, bool hasCustom)
{
	if (hasCustom == hasCutomEmptyValues(colName))
		return;

	if (hasCustom)
	{
		_customEmptyValuesPerColumn[colName] = _workspaceEmptyValues;
		_customDoubleEmptyValuesPerColumn[colName] = _workspaceDoubleEmptyValues;
	}
	else
	{
		_customEmptyValuesPerColumn.erase(colName);
		_customDoubleEmptyValuesPerColumn.erase(colName);
	}
}


