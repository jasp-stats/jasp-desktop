#include "jsonutilities.h"

std::set<std::string> JsonUtilities::convertEasyFilterJSONToSet(std::string jsonStr)
{
	std::set<std::string> returnThis;

	Json::Reader reader;
	Json::Value json;
	reader.parse(jsonStr, json);

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
