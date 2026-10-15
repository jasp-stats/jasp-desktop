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

bool JsonUtilities::jsonValuesEqual(const Json::Value & left, const Json::Value & right)
{
	//Numeric-tolerant: values round-tripped through e.g. the plot editor can swap
	//between int and double representation (1 vs 1.0), which jsoncpp's operator==
	//would report as a difference.
	if(left.isNumeric() && right.isNumeric())
		return left.asDouble() == right.asDouble();

	if(left.type() != right.type())
		return false;

	if(left.isObject())
	{
		if(left.size() != right.size())
			return false;

		for(const std::string & key : left.getMemberNames())
			if(!right.isMember(key) || !jsonValuesEqual(left[key], right[key]))
				return false;

		return true;
	}

	if(left.isArray())
	{
		if(left.size() != right.size())
			return false;

		for(Json::ArrayIndex i = 0; i < left.size(); i++)
			if(!jsonValuesEqual(left[i], right[i]))
				return false;

		return true;
	}

	//strings, booleans, null
	return left == right;
}

Json::Value JsonUtilities::jsonDiff(const Json::Value & before, const Json::Value & after)
{
	Json::Value patch = Json::objectValue;

	if(!before.isObject() || !after.isObject())
		return jsonValuesEqual(before, after) ? patch : after;

	for(const std::string & key : after.getMemberNames())
	{
		if(!before.isMember(key))
			patch[key] = after[key];
		else if(before[key].isObject() && after[key].isObject())
		{
			Json::Value subPatch = jsonDiff(before[key], after[key]);
			if(subPatch.isObject() && subPatch.size() > 0)
				patch[key] = subPatch;
		}
		else if(!jsonValuesEqual(before[key], after[key]))
			patch[key] = after[key];
	}

	for(const std::string & key : before.getMemberNames())
		if(!after.isMember(key))
			patch[key] = Json::nullValue; //RFC 7386: null means "remove" when applied

	return patch;
}

void JsonUtilities::jsonApplyPatch(Json::Value & target, const Json::Value & patch)
{
	if(!patch.isObject())
	{
		target = patch;
		return;
	}

	if(!target.isObject())
		target = Json::objectValue;

	for(const std::string & key : patch.getMemberNames())
	{
		const Json::Value & patchValue = patch[key];

		if(patchValue.isNull())
			target.removeMember(key);
		else if(patchValue.isObject() && target.isMember(key) && target[key].isObject())
			jsonApplyPatch(target[key], patchValue);
		else
			target[key] = patchValue;
	}
}

Json::Value JsonUtilities::jsonKeysMissingFrom(const Json::Value & source, const Json::Value & reference)
{
	if(!source.isObject())
		return Json::objectValue;

	Json::Value missing = Json::objectValue;

	for(const std::string & key : source.getMemberNames())
	{
		if(!reference.isObject() || !reference.isMember(key))
			missing[key] = source[key];
		else if(source[key].isObject() && reference[key].isObject())
		{
			Json::Value subMissing = jsonKeysMissingFrom(source[key], reference[key]);
			if(subMissing.isObject() && subMissing.size() > 0)
				missing[key] = subMissing;
		}
		//else: the key exists in both, so it cannot be told apart from engine
		//output and is dropped (this is deliberately presence-based, values are
		//not compared)
	}

	return missing;
}
