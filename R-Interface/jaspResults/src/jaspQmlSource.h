#ifndef JASPQMLSOURCE_H
#define JASPQMLSOURCE_H

#include "jaspTable.h"

class jaspQmlSource : public jaspTable
{
public:
					jaspQmlSource(const std::string & sourceID = "");

	void			setSourceID(const std::string & sourceID)			{ _sourceID = sourceID; }
	std::string		sourceID()									const	{ return _sourceID; }

	Json::Value	metaEntry()								const	override { return constructMetaEntry("qmlSource"); }
	Json::Value	dataEntry(std::string & errorMessage)	const	override;

	std::string		_sourceID;
};


class jaspQmlSource_Interface : public jaspTable_Interface
{
public:
	jaspQmlSource_Interface(jaspObject * dataObj) : jaspTable_Interface(dataObj) {}

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspQmlSource, std::string,	_sourceID,	SourceID)
};

RCPP_EXPOSED_CLASS_NODECL(jaspQmlSource_Interface)


#endif // JASPQMLSOURCE_H
