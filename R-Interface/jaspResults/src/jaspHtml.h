#pragma once
#include "jaspObject.h"

class jaspHtml : public jaspObject
{
public:
  jaspHtml(std::string text = "", std::string elementType = "p", std::string maxWidth="15cm", std::string Class = "") : jaspObject(jaspObjectType::html, ""), _rawText(text), _elementType(elementType), _class(Class), _maxWidth(maxWidth) {}

	~jaspHtml() {}

	std::string dataToString(std::string prefix="")			const	override;
	std::string toHtml()											override;

	Json::Value	metaEntry()									const	override { return constructMetaEntry("htmlNode"); }
	Json::Value	dataEntry(std::string & errorMessage)		const	override;

	std::string _rawText, _elementType, _class, _maxWidth;

	Json::Value convertToJSON()								const	override;
	void		convertFromJSON_SetFields(Json::Value in)			override;

	std::string convertTextToHtml(const std::string text)	const;

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
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspHtml, std::string,	_maxWidth,		MaxWidth)

};

RCPP_EXPOSED_CLASS_NODECL(jaspHtml_Interface)

