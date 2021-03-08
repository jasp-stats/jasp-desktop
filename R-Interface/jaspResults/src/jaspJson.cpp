#include "jaspJson.h"

Json::Value jaspJson::RObject_to_JsonValue(Rcpp::RObject obj)
{
	if(obj.isNULL())								return Json::nullValue;
	else if(Rcpp::is<Rcpp::List>(obj))				return RObject_to_JsonValue((Rcpp::List)					obj);
	else if(Rcpp::is<Rcpp::DataFrame>(obj))			return RObject_to_JsonValue((Rcpp::List)					obj);
	else if(Rcpp::is<Rcpp::NumericMatrix>(obj))		return RObject_to_JsonValue<REALSXP>((Rcpp::NumericMatrix)	obj);
	else if(Rcpp::is<Rcpp::NumericVector>(obj))		return RObject_to_JsonValue<REALSXP>((Rcpp::NumericVector)	obj);
	else if(Rcpp::is<Rcpp::IntegerVector>(obj))		return RObject_to_JsonValue<INTSXP>((Rcpp::IntegerVector)	obj);
	else if(Rcpp::is<Rcpp::LogicalVector>(obj))		return RObject_to_JsonValue<LGLSXP>((Rcpp::LogicalVector)	obj);
	else if(Rcpp::is<Rcpp::CharacterVector>(obj))	return RObject_to_JsonValue<STRSXP>((Rcpp::CharacterVector)	obj);
	else if(Rcpp::is<Rcpp::StringVector>(obj))		return RObject_to_JsonValue<STRSXP>((Rcpp::StringVector)	obj);
	else if(obj.isS4())								return "an S4, which is too complicated for jaspResults now.";
	else											return "something that is not understood by jaspResults right now..";
}

Json::Value jaspJson::RObject_to_JsonValue(Rcpp::List obj)
{
	bool atLeastOneNamed = false;

	Rcpp::RObject namesListRObject = obj.names();
	Rcpp::CharacterVector namesList;

	if(!namesListRObject.isNULL())
	{
		namesList = namesListRObject;

		for(int row=0; row<obj.size(); row++)
			if(namesList[row] != "")
				atLeastOneNamed = true;
	}

	Json::Value val = atLeastOneNamed ? Json::objectValue : Json::arrayValue;

	if(atLeastOneNamed)
		for(int row=obj.size() - 1; row>=0; row--) //We go backwards because in R the first entry of a name in a list is used. So to emulate this we go backwars and we override an earlier occurence. (aka you have two elements with the name "a" in a list and in R list$a returns the first occurence. This is now also the element visible in the json.)
		{
			std::string name(namesList[row]);

			if(name == "")
				name = "element_" + std::to_string(row);

			val[name] = RObject_to_JsonValue((Rcpp::RObject)obj[row]);
		}
	else
		for(int row=0; row<obj.size(); row++)
			val.append(RObject_to_JsonValue((Rcpp::RObject)obj[row]));


	return val;
}

std::string jaspJson::jsonToPrefixedStrings(Json::Value val, std::string prefix)
{
	if(prefix == "")
		return val.toStyledString();

	std::string styled = val.toStyledString();

	std::stringstream out;

	for(char letter : styled)
		if(letter != '\n')
			out << letter;
		else
			out << letter << prefix;

	return out.str();
}

Json::Value jaspJson::VectorJson_to_ArrayJson(std::vector<Json::Value> vec)
{
	Json::Value array(Json::arrayValue);
	for(auto val: vec)
		array.append(val);
	return array;
}

Json::Value jaspJson::SetJson_to_ArrayJson(std::set<Json::Value> set)
{
	Json::Value array(Json::arrayValue);
	for(auto val: set)
		array.append(val);
	return array;
}

std::set<Json::Value> jaspJson::ArrayJson_to_SetJson(Json::Value arr)
{
	std::set<Json::Value> set;
	for(auto & val: arr)
		set.insert(val);
	return set;
}

std::vector<Json::Value> jaspJson::RList_to_VectorJson(Rcpp::List obj)
{
	std::vector<Json::Value> vec;

	for(int row=0; row<obj.size(); row++)
		vec.push_back(RObject_to_JsonValue((Rcpp::RObject)obj[row]));

	return vec;
}

Json::Value jaspJson::dataEntry(std::string & errorMessage) const
{
	Json::Value data(jaspObject::dataEntry(errorMessage));

	data["title"]		= _title;
	data["json"]		= _json;
	data["name"]		= getUniqueNestedName();

	return data;
}

Json::Value jaspJson::convertToJSON() const
{
	Json::Value obj = jaspObject::convertToJSON();
	obj["json"]		= _json;

	return obj;
}

void jaspJson::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_json = in.get("json", Json::nullValue);
}
