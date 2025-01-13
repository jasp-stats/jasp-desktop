#ifndef JSONUTILITIES_H
#define JSONUTILITIES_H

#include <json/json.h>
#include <string>
#include <set>
#include <cmath>
#include "utils.h"

/// There are recurring needs when working with Json::Value etc and these are collected here.
/// For instance converting from and to a std::vector<std::string> is useful and might as well be written here.
/// The function vecToJsonArray for instance handles inf or nan occuring in double, because json doesn't support it...
class JsonUtilities
{
public:
	static std::set<std::string>	convertDragNDropFilterJSONToSet(std::string jsonStr);

	static std::string				removeColumnsFromDragNDropFilterJSONStr(	const	std::string & jsonStr,	const stringset & columnNames);
	static void						removeColumnsFromDragNDropFilterJSONRef(			Json::Value & json,		const stringset & columnNames);
	static Json::Value				removeColumnsFromDragNDropFilterJSON(		const	Json::Value & json,		const stringset & columnNames);
	static std::string				removeColumnsFromDragNDropFilterJSONStr(	const	std::string & jsonStr,	const stringvec & columnNames);
	static void						removeColumnsFromDragNDropFilterJSONRef(			Json::Value & json,		const stringvec & columnNames);
	static Json::Value				removeColumnsFromDragNDropFilterJSON(		const	Json::Value & json,		const stringvec & columnNames);

	static std::string				replaceColumnNamesInDragNDropFilterJSONStr(	const	std::string & jsonStr,	const strstrmap & changeNameColumns);
	static void						replaceColumnNamesInDragNDropFilterJSONRef(			Json::Value & json,		const strstrmap & changeNameColumns);
	static Json::Value				replaceColumnNamesInDragNDropFilterJSON(	const	Json::Value & json,		const strstrmap & changeNameColumns);

	static stringvec				jsonStringArrayToVec(const Json::Value & jsonStrings);
	static stringset				jsonStringArrayToSet(const Json::Value & jsonStrings);

	template<typename T>
	static Json::Value				vecToJsonArray(const std::vector<T> & vec)
	{
		Json::Value out = Json::arrayValue;

		for(const T & e : vec)	out.append(e);

		return out;
	}
	
	template<typename T> static Json::Value				setToJsonArray(const std::set<T> & set) { return vecToJsonArray<T>(std::vector<T>(set.begin(), set.end())); }
	
	static Json::Value				vecToJsonArray(const std::vector<double> vec)
	{
		Json::Value out = Json::arrayValue;

		for(const double & v : vec)
			if(std::isnan(v) || std::isinf(v))	out.append(Json::nullValue); //Json does not support inf or nan... Sigh...
			else								out.append(v);

		return out;
	}

/*	static Json::Value				vecToJsonArray(const std::vector<QString> & vec)
	{
		Json::Value out = Json::arrayValue;

		for(const QString & e : vec)	out.append(fq(e));

		return out;
	}*/


private:
	JsonUtilities() {}
};

#endif // JSONUTILITIES_H
