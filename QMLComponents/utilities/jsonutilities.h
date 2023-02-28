#ifndef JSONUTILITIES_H
#define JSONUTILITIES_H

#include <json/json.h>
#include <string>
#include <set>
#include "stringutils.h"
#include <cmath>
#include "qutils.h"

/// There are recurring needs when working with Json::Value etc and these are collected here.
/// For instance converting from and to a std::vector<std::string> is useful and might as well be written here.
/// The function vecToJsonArray for instance handles inf or nan occuring in double, because json doesn't support it...
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

	template<typename T>
	static Json::Value				vecToJsonArray(const std::vector<T> & vec)
	{
		Json::Value out = Json::arrayValue;

		for(const T & e : vec)	out.append(e);

		return out;
	}

	static Json::Value				vecToJsonArray(const std::vector<double> vec)
	{
		Json::Value out = Json::arrayValue;

		for(const double & v : vec)
			if(std::isnan(v) || std::isinf(v))	out.append(Json::nullValue); //Json does not support inf or nan... Sigh...
			else								out.append(v);

		return out;
	}

	static Json::Value				vecToJsonArray(const std::vector<QString> & vec)
	{
		Json::Value out = Json::arrayValue;

		for(const QString & e : vec)	out.append(fq(e));

		return out;
	}


private:
	JsonUtilities() {}
};

#endif // JSONUTILITIES_H
