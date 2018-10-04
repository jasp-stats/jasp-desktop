#pragma once
#include "jaspObject.h"

class jaspHtml : public jaspObject
{
public:
  jaspHtml(std::string text = "", std::string elementType = "p", std::string Class = "") : jaspObject(jaspObjectType::html, ""), _rawText(text), _elementType(elementType), _class(Class) {}

	~jaspHtml() {}

	std::string dataToString(std::string prefix="") override;

	Json::Value	metaEntry() override { return constructMetaEntry("htmlNode"); }
	Json::Value	dataEntry() override;

    std::string _rawText, _elementType, _class;

	Json::Value convertToJSON() override;
	void		convertFromJSON_SetFields(Json::Value in) override;

    std::string convertTextToHtml(const std::string text);

    void setText(std::string newRawText);
    std::string getText();
    std::string getHtml();
};



class jaspHtml_Interface : public jaspObject_Interface
{
public:
	jaspHtml_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

    void 		setText(std::string newRawText) { 			static_cast<jaspHtml *>(myJaspObject)->setText(newRawText); }
    std::string getText() 						{ return 	static_cast<jaspHtml *>(myJaspObject)->getText(); }
    std::string getHtml()						{ return	static_cast<jaspHtml *>(myJaspObject)->getHtml(); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspHtml, std::string,	_elementType,	ElementType)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspHtml, std::string,	_class,			Class)

};

RCPP_EXPOSED_CLASS_NODECL(jaspHtml_Interface)

