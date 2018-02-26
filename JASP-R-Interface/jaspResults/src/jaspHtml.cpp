#include "jaspHtml.h"


std::string jaspHtml::dataToString(std::string prefix)
{
	std::stringstream out;
	out << "<" << _elementType  << ">" << _text << " </" << _elementType << ">";
	return out.str();
}

Json::Value jaspHtml::dataEntry()
{
	Json::Value data(Json::objectValue);

	data["title"]		= _title;
	data["text"]		= _text;
	data["elementType"]	= _elementType;
	data["name"]		= getUniqueNestedName();

	return data;
}


Json::Value jaspHtml::convertToJSON()
{
	Json::Value obj		= jaspObject::convertToJSON();
	obj["text"]			= _text;
	obj["elementType"]	= _elementType;

	return obj;
}

void jaspHtml::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_text			= in.get("text",		"null").asString();
	_elementType	= in.get("elementType", "null").asString();
}
