#ifndef JASPQMLSOURCE_H
#define JASPQMLSOURCE_H

#include "jaspJson.h"

class jaspQmlSource : public jaspJson
{
public:
					jaspQmlSource(const std::string & sourceID = "");

	void			setSourceID(const std::string & sourceID)			{ _sourceID = sourceID; }
	std::string		sourceID()									const	{ return _sourceID; }

	Json::Value	metaEntry()								const	override { return constructMetaEntry("qmlSource"); }
	Json::Value	dataEntry(std::string & errorMessage)	const	override;

	void		convertFromJSON_SetFields(Json::Value in)		override;
	Json::Value convertToJSON()							const	override;

	std::string		_sourceID;
};


class jaspQmlSource_Interface : public jaspJson_Interface
{
public:
	jaspQmlSource_Interface(jaspObject * dataObj) : jaspJson_Interface(dataObj) {}

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspQmlSource, std::string,	_sourceID,	SourceID)

	void			setValue(Rcpp::RObject obj)			{ ((jaspQmlSource*)myJaspObject)->setValue(obj);		}
	std::string		getValue()							{ return ((jaspQmlSource*)myJaspObject)->getValue();	}
};

RCPP_EXPOSED_CLASS_NODECL(jaspQmlSource_Interface)


#endif // JASPQMLSOURCE_H
