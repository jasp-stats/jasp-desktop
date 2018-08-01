#include "dynamicmodule.h"


namespace Modules
{
AnalysisEntry * RibbonEntry::firstAnalysisEntry()
{
	if(_analysisEntries.size() == 0)
		throw std::runtime_error("RibbonEntry has no entries!");
	else
		return _analysisEntries[0];
}

AnalysisEntry* RibbonEntry::retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile)
{
	std::string ribbonTitle = jsonFromJaspFile.get("ribbonEntry", "RibbonEntry's title wasn't actually specified!").asString();

	if(title() != ribbonTitle)
		throw ModuleException(dynamicModule()->name(), "Tried to load an AnalysisEntry for RibbonEntry (" + ribbonTitle +") from RibbonEntry ("+title()+")...");

	std::string analysisTitle = jsonFromJaspFile.get("analysisEntry", "AnalysisEntry's title wasn't actually specified!").asString();

	for(AnalysisEntry * entry : _analysisEntries)
		if(entry->title() == analysisTitle)
			return entry;

	throw ModuleException(dynamicModule()->name(), "Couldn't find AnalysisEntry " + analysisTitle);
}

}
