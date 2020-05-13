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

std::string JsonUtilities::removeColumnsFromDragNDropFilterJSON(const std::string & jsonStr, const std::vector<std::string> & columnNames)
{
	return removeColumnsFromDragNDropFilterJSON(jsonStr, std::set<std::string>(columnNames.begin(), columnNames.end()));
}

std::string JsonUtilities::removeColumnsFromDragNDropFilterJSON(const std::string & jsonStr, const std::set<std::string> & columnNames)
{
	Json::Value json;
	Json::Reader().parse(jsonStr, json);

	removeColumnsFromDragNDropFilterJSON(json, columnNames);

	return json.toStyledString();
}

void JsonUtilities::removeColumnsFromDragNDropFilterJSON(Json::Value & json,  const std::vector<std::string> & columnNames)
{
	removeColumnsFromDragNDropFilterJSON(json, std::set<std::string>(columnNames.begin(), columnNames.end()));
}

void JsonUtilities::removeColumnsFromDragNDropFilterJSON(Json::Value & json, const std::set<std::string> & columnNames)
{
	if(json.isArray())
		for(int i=0; i<json.size(); i++)
			removeColumnsFromDragNDropFilterJSON(json[i], columnNames);

	else if(json.isObject())
	{
		if(json.get("nodeType", "").asString() == "Column" && columnNames.count(json["columnName"].asString()) > 0)
			json = Json::nullValue;
		else
			for(auto & key : json.getMemberNames())
				removeColumnsFromDragNDropFilterJSON(json[key], columnNames);
	}
}

std::string JsonUtilities::replaceColumnNamesInDragNDropFilterJSON(const std::string & jsonStr, const std::map<std::string, std::string> & changeNameColumns)
{
	Json::Value json;
	Json::Reader().parse(jsonStr, json);

	replaceColumnNamesInDragNDropFilterJSON(json, changeNameColumns);

	return json.toStyledString();
}

void JsonUtilities::replaceColumnNamesInDragNDropFilterJSON(Json::Value & json, const std::map<std::string, std::string> & changeNameColumns)
{
	if(json.isArray())
		for(int i=0; i<json.size(); i++)
			replaceColumnNamesInDragNDropFilterJSON(json[i], changeNameColumns);
	else if(json.isObject())
	{
		if(json.get("nodeType", "").asString() == "Column" && changeNameColumns.count(json["columnName"].asString()) > 0)
			json["columnName"] = changeNameColumns.at(json["columnName"].asString());
		else
			for(auto & key : json.getMemberNames())
				replaceColumnNamesInDragNDropFilterJSON(json[key], changeNameColumns);
	}
}
