#ifndef RIBBONENTRY_H
#define RIBBONENTRY_H

#include <string>
#include <vector>
#include "jsonredirect.h"
#include "analysisentry.h"

namespace Modules
{
class DynamicModule;

class RibbonEntry
{
public:
	RibbonEntry(Json::Value & ribbonEntry, DynamicModule * parentDynamicModule) :
		_title(ribbonEntry.get("title",	"???").asString()),
		_icon( ribbonEntry.get("icon",	"???").asString()),
		_dynamicModule(parentDynamicModule)
	{
		for(Json::Value & analysisEntry : ribbonEntry["analyses"])
			_analysisEntries.push_back(new AnalysisEntry(analysisEntry, this));
	}

	~RibbonEntry()
	{
		for(auto * entry : _analysisEntries)
			delete entry;
		_analysisEntries.clear();
	}

	std::string							title()				const	{ return _title;				}
	std::string							icon()				const	{ return _icon;				}
	const std::vector<AnalysisEntry*>&	analysisEntries()	const	{ return _analysisEntries;	}
	DynamicModule*						dynamicModule()		const	{ return _dynamicModule;		}

	AnalysisEntry*						firstAnalysisEntry();
	AnalysisEntry*						retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile);

private:
	std::string					_title,
								_icon;

	std::vector<AnalysisEntry*>	_analysisEntries;
	DynamicModule				*_dynamicModule = NULL;
};

}

#endif // RIBBONENTRY_H
