#include "jaspHtml.h"


std::string jaspHtml::dataToString(std::string prefix) const
{
    return convertTextToHtml(_rawText);
}

std::string jaspHtml::convertTextToHtml(std::string text) const
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
	if(_elementType != "" && _elementType != "errorMsg")
		out << "<" << _elementType  << (_class != "" ? "class=\""+_class+'"' : "") << ">";

    out << text;

	if(_elementType != "" && _elementType != "errorMsg")
		out << " </" << _elementType << ">";

    return out.str();
}

std::string jaspHtml::toHtml()
{

	return (_elementType != "errorMsg" ? "<div class=\"jaspHtml\" style=\"max-width:" + _maxWidth + ";\">\n" : "<div class=\"analysis-error-message error-message-box ui-state-error\"><span class=\"ui-icon ui-icon-alert\" style=\"float: left; margin-right: .3em;\"/>\n" )
			+ htmlTitle() + "\n" + dataToString() + "</div>" "\n";

}

Json::Value jaspHtml::dataEntry(std::string & errorMessage) const
{
	Json::Value data(jaspObject::dataEntry(errorMessage));

    data["rawtext"]		= _rawText;
    data["text"]		= convertTextToHtml(_rawText);
	data["title"]		= _title;
	data["class"]		= _class;
	data["maxWidth"]	= _maxWidth;
	data["elementType"]	= _elementType;
	data["name"]		= getUniqueNestedName();

	return data;
}


Json::Value jaspHtml::convertToJSON() const
{
	Json::Value obj		= jaspObject::convertToJSON();
    obj["rawtext"]		= _rawText;
    obj["text"]			= convertTextToHtml(_rawText);
	obj["class"]		= _class;
	obj["maxWidth"]		= _maxWidth;
	obj["elementType"]	= _elementType;

	return obj;
}

void jaspHtml::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

    _rawText		= in.get("rawtext",		"null").asString();
	_class			= in.get("class",		"null").asString();
	_maxWidth		= in.get("maxWidth",	"10cm").asString();
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
