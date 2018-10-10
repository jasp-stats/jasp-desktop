#include "jaspHtml.h"


std::string jaspHtml::dataToString(std::string prefix)
{
    return convertTextToHtml(_rawText);
}

std::string jaspHtml::convertTextToHtml(std::string text)
{
    // @vankesteren with help from https://stackoverflow.com/a/24315631 and @jorisgoosen

    // First replace the newlines with <br/>
    const std::string 	from	= "\n",
                        to		= "<br/>";
    size_t start_pos = 0;

    while((start_pos = text.find(from, start_pos)) != std::string::npos) {
        text.replace(start_pos, from.length(), to);
        start_pos += to.length(); // Handles case where 'to' is a substring of 'from'
    }

    // Then add element tags
    std::stringstream out;
    if(_elementType != "")
        out << "<" << _elementType  << (_class != "" ? "class=\""+_class+'"' : "") << ">";

    out << text;

    if(_elementType != "")
        out << " </" << _elementType << ">";

    return out.str();
}

std::string jaspHtml::toHtml()
{
	return "<div class=\"jaspHtml\">" "\n" + htmlTitle() + "\n" + dataToString() + "</div>" "\n";
}

Json::Value jaspHtml::dataEntry()
{
	Json::Value data(jaspObject::dataEntry());

    data["rawtext"]		= _rawText;
    data["text"]		= convertTextToHtml(_rawText);
	data["title"]		= _title;
	data["class"]		= _class;
	data["elementType"]	= _elementType;
	data["name"]		= getUniqueNestedName();

	return data;
}


Json::Value jaspHtml::convertToJSON()
{
	Json::Value obj		= jaspObject::convertToJSON();
    obj["rawtext"]		= _rawText;
    obj["text"]			= convertTextToHtml(_rawText);
	obj["class"]		= _class;
	obj["elementType"]	= _elementType;

	return obj;
}

void jaspHtml::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

    _rawText		= in.get("rawtext",		"null").asString();
	_class			= in.get("class",		"null").asString();
	_elementType	= in.get("elementType", "null").asString();
}

void jaspHtml::setText(std::string newRawText) {
    _rawText 	= newRawText;
}

std::string jaspHtml::getText() {
    return _rawText;
}

std::string jaspHtml::getHtml() {
    return convertTextToHtml(_rawText);
}
