#include "jsonutilities.h"

std::set<std::string> JsonUtilities::convertDragNDropFilterJSONToSet(std::string jsonStr)
{
	std::set<std::string> returnThis;

	Json::Value json;
	Json::Reader().parse(jsonStr, json);

	std::stack<Json::Value> jsonStack;
	jsonStack.push(json);

	while(jsonStack.size() > 0)
	{
		Json::Value cur = jsonStack.top();
		jsonStack.pop();

		if(cur.isArray())
			for(int i=0; i<cur.size(); i++)
				jsonStack.push(cur[i]);
		else if(cur.isObject())
		{
			if(cur.get("nodeType", "").asString() == "Column")
				returnThis.insert(cur.get("columnName", "").asString());
			else
				for(auto & str : cur.getMemberNames())
					jsonStack.push(cur[str]);
		}
	}

	return returnThis;
}

std::string JsonUtilities::removeColumnsFromDragNDropFilterJSONStr(const std::string & jsonStr, const std::vector<std::string> & columnNames)
{
	return removeColumnsFromDragNDropFilterJSONStr(jsonStr, std::set<std::string>(columnNames.begin(), columnNames.end()));
}

std::string JsonUtilities::removeColumnsFromDragNDropFilterJSONStr(const std::string & jsonStr, const std::set<std::string> & columnNames)
{
	Json::Value json;
	Json::Reader().parse(jsonStr, json);

	removeColumnsFromDragNDropFilterJSON(json, columnNames);

	return json.toStyledString();
}

void JsonUtilities::removeColumnsFromDragNDropFilterJSONRef(Json::Value & json,  const std::vector<std::string> & columnNames)
{
	removeColumnsFromDragNDropFilterJSONRef(json, std::set<std::string>(columnNames.begin(), columnNames.end()));
}

void JsonUtilities::removeColumnsFromDragNDropFilterJSONRef(Json::Value & json, const std::set<std::string> & columnNames)
{
	if(json.isArray())
		for(int i=0; i<json.size(); i++)
			removeColumnsFromDragNDropFilterJSONRef(json[i], columnNames);

	else if(json.isObject())
	{
		if(json.get("nodeType", "").asString() == "Column" && columnNames.count(json["columnName"].asString()) > 0)
			json = Json::nullValue;
		else
			for(auto & key : json.getMemberNames())
				removeColumnsFromDragNDropFilterJSONRef(json[key], columnNames);
	}
}

Json::Value JsonUtilities::removeColumnsFromDragNDropFilterJSON(const Json::Value &json, const stringset &columnNames)
{
	Json::Value changeThisThen = json;
	removeColumnsFromDragNDropFilterJSONRef(changeThisThen, columnNames);
	return changeThisThen;
}

Json::Value JsonUtilities::removeColumnsFromDragNDropFilterJSON(const Json::Value &json, const stringvec &columnNames)
{
	Json::Value changeThisThen = json;
	removeColumnsFromDragNDropFilterJSONRef(changeThisThen, columnNames);
	return changeThisThen;
}

std::string JsonUtilities::replaceColumnNamesInDragNDropFilterJSONStr(const std::string & jsonStr, const std::map<std::string, std::string> & changeNameColumns)
{
	Json::Value json;
	Json::Reader().parse(jsonStr, json);

	replaceColumnNamesInDragNDropFilterJSON(json, changeNameColumns);

	return json.toStyledString();
}

void JsonUtilities::replaceColumnNamesInDragNDropFilterJSONRef(Json::Value & json, const std::map<std::string, std::string> & changeNameColumns)
{
	if(json.isArray())
		for(int i=0; i<json.size(); i++)
			replaceColumnNamesInDragNDropFilterJSONRef(json[i], changeNameColumns);

	else if(json.isObject())
	{
		if(json.get("nodeType", "").asString() == "Column" && changeNameColumns.count(json["columnName"].asString()) > 0)
			json["columnName"] = changeNameColumns.at(json["columnName"].asString());
		else
			for(auto & key : json.getMemberNames())
				replaceColumnNamesInDragNDropFilterJSONRef(json[key], changeNameColumns);
	}
}

Json::Value JsonUtilities::replaceColumnNamesInDragNDropFilterJSON(const Json::Value &json, const strstrmap &changeNameColumns)
{
	Json::Value changeThisThen = json;
	replaceColumnNamesInDragNDropFilterJSONRef(changeThisThen, changeNameColumns);
	return changeThisThen;
}

stringvec JsonUtilities::jsonStringArrayToVec(const Json::Value & jsonStrings)
{
	if(jsonStrings.isNull())	return {};
	if(!jsonStrings.isArray())	throw std::runtime_error("jsonStringArrayToVec expected array!");

	stringvec out;
	out.reserve(jsonStrings.size());

	for(const Json::Value & entry: jsonStrings)
		if(!entry.isString())	throw std::runtime_error("jsonStringArrayToVec expected array of strings!");
		else					out.push_back(entry.asString());

	return out;
}

stringset JsonUtilities::jsonStringArrayToSet(const Json::Value &jsonStrings)
{
	auto vec = jsonStringArrayToVec(jsonStrings);
	
	return stringset(vec.begin(), vec.end());
}
