#include "jaspHtml.h"


std::string jaspHtml::dataToString(std::string prefix)
{
	std::stringstream out;
	if(_elementType != "")
		out << "<" << _elementType  << (_class != "" ? "class=\""+_class+'"' : "") << ">";

	out << _text;

	if(_elementType != "")
		out << " </" << _elementType << ">";

	return out.str();
}

Json::Value jaspHtml::dataEntry()
{
	Json::Value data(jaspObject::dataEntry());

	data["text"]		= _text;
	data["class"]		= _class;
	data["elementType"]	= _elementType;
	data["name"]		= getUniqueNestedName();

	return data;
}


Json::Value jaspHtml::convertToJSON()
{
	Json::Value obj		= jaspObject::convertToJSON();
	obj["text"]			= _text;
	obj["class"]		= _class;
	obj["elementType"]	= _elementType;

	return obj;
}

void jaspHtml::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_text			= in.get("text",		"null").asString();
	_class			= in.get("class",		"null").asString();
	_elementType	= in.get("elementType", "null").asString();
}
