#pragma once
#include "jaspObject.h"

class jaspPlot : public jaspObject
{
public:
	jaspPlot(std::string title = "") : jaspObject(jaspObjectType::plot, title) { initEnvName(); }

	~jaspPlot();

	void addFootnote(std::string message, std::string symbol = "");

	float		_aspectRatio;
	int			_width,
				_height;
	std::string	_filePathPng,
				_status = "waiting",
				_envName;

	///For safekeeping (aka state replacement?)
	void setPlotObject(Rcpp::RObject plotSerialized);
	Rcpp::RObject getPlotObject();

	std::string dataToString(std::string prefix) override;

	Json::Value	metaEntry() override { return constructMetaEntry("image"); }
	Json::Value	dataEntry() override;
	std::string toHtml()	override;

	Json::Value convertToJSON() override;
	void		convertFromJSON_SetFields(Json::Value in) override;

private:
	void initEnvName();

	//Rcpp::Vector<RAWSXP> _plotObjSerialized;
	Json::Value _footnotes = Json::arrayValue;
};


class jaspPlot_Interface : public jaspObject_Interface
{
public:
	jaspPlot_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void addFootnote(std::string message)	{ ((jaspPlot*)myJaspObject)->addFootnote(message); }
	void setPlotObject(Rcpp::RObject plotObject)					{ ((jaspPlot*)myJaspObject)->setPlotObject(plotObject); }
	Rcpp::RObject getPlotObject()									{ return ((jaspPlot*)myJaspObject)->getPlotObject(); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, std::string,	_filePathPng,	FilePathPng)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, std::string,	_status,		Status)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, float,			_aspectRatio,	AspectRatio)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_width,			Width)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_height,		Height)
};

RCPP_EXPOSED_CLASS_NODECL(jaspPlot_Interface)
