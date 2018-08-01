#include "dynamicmodule.h"

namespace Modules
{

DynamicModule*	AnalysisEntry::dynamicModule() const
{
	return _ribbonEntry->dynamicModule();
}

std::string AnalysisEntry::qmlFilePath() const
{
	return dynamicModule()->qmlFilePath(_qml);
}

std::string AnalysisEntry::getFullRCall() const
{
	return dynamicModule()->rModuleCall(_function);
}

Json::Value AnalysisEntry::getDefaultResults() const
{
	Json::Value res(Json::objectValue),
				metaEnt(Json::objectValue);

	res["title"]			= title();
	res[".meta"]			= Json::arrayValue;
	res["notice"]			= Json::objectValue;
	res["notice"]["title"]	= "Waiting for intialization of module: " + dynamicModule()->title();
	res["notice"]["height"] = 0;
	res["notice"]["width"]	= 0;

	metaEnt["name"]			= "notice";
	metaEnt["type"]			= "image"; //pretending it is a plot to make it show up at least, width == height == 0 to make sure no space is wasted
	res[".meta"].append(metaEnt);

	return res;
}

Json::Value AnalysisEntry::asJsonForJaspFile()	const
{
	Json::Value json(Json::objectValue);

	json["moduleName"]			= dynamicModule()->name();
	json["moduleVersion"]		= dynamicModule()->version();
	json["moduleMaintainer"]	= dynamicModule()->maintainer();
	json["moduleWebsite"]		= dynamicModule()->website();
	json["analysisEntry"]		= _title;
	json["ribbonEntry"]			= _ribbonEntry->title();

	return json;
}

} // namespace Modules
