#ifndef JSONUTILITIES_H
#define JSONUTILITIES_H

#include "jsonredirect.h"
#include <string>
#include <set>
#include "stringutils.h"


class JsonUtilities
{
public:
	static std::set<std::string>	convertDragNDropFilterJSONToSet(std::string jsonStr);

	static std::string				removeColumnsFromDragNDropFilterJSON(const std::string & jsonStr,		const std::set<std::string> & columnNames);
	static void						removeColumnsFromDragNDropFilterJSON(Json::Value & json,				const std::set<std::string> & columnNames);
	static std::string				removeColumnsFromDragNDropFilterJSON(const std::string & jsonStr,		const std::vector<std::string> & columnNames);
	static void						removeColumnsFromDragNDropFilterJSON(Json::Value & json,				const std::vector<std::string> & columnNames);

	static std::string				replaceColumnNamesInDragNDropFilterJSON(const std::string & jsonStr,	const std::map<std::string, std::string> & changeNameColumns);
	static void						replaceColumnNamesInDragNDropFilterJSON(Json::Value & json,				const std::map<std::string, std::string> & changeNameColumns);

	static stringvec				jsonStringArrayToVec(Json::Value & jsonStrings);

private:
	JsonUtilities() {}
};

#endif // JSONUTILITIES_H
