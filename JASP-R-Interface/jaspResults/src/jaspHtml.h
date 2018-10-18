#pragma once
#include "jaspObject.h"

class jaspHtml : public jaspObject
{
public:
  jaspHtml(std::string text = "", std::string elementType = "p", std::string Class = "") : jaspObject(jaspObjectType::html, ""), _text(text), _elementType(elementType), _class(Class) {}

	~jaspHtml() {}

	std::string dataToString(std::string prefix="") override;

	Json::Value	metaEntry() override { return constructMetaEntry("htmlNode"); }
	Json::Value	dataEntry() override;

	std::string _text, _elementType, _class;

	Json::Value convertToJSON() override;
	void		convertFromJSON_SetFields(Json::Value in) override;
};



class jaspHtml_Interface : public jaspObject_Interface
{
public:
	jaspHtml_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}


	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspHtml, std::string,	_text,			Text)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspHtml, std::string,	_elementType,	ElementType)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspHtml, std::string,	_class,			Class)

};

RCPP_EXPOSED_CLASS_NODECL(jaspHtml_Interface)

