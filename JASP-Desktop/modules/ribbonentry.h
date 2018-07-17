#ifndef RIBBONENTRY_H
#define RIBBONENTRY_H

#include <string>
#include <vector>
#include "jsonredirect.h"
#include "analysisentry.h"

namespace Modules
{

class RibbonEntry
{
public:
	RibbonEntry(Json::Value & ribbonEntry) :
		_title(ribbonEntry.get("title",	"???").asString()),
		_icon(ribbonEntry.get("icon",	"???").asString())
	{
		for(Json::Value & analysisEntry : ribbonEntry["analyses"])
			_analysisEntries.push_back(analysisEntry);
	}

	std::string							title()				const { return _title; }
	std::string							icon()				const { return _icon; }
	const std::vector<AnalysisEntry> &	analysisEntries()	const { return _analysisEntries; }

private:
	std::string					_title,
								_icon;

	std::vector<AnalysisEntry>	_analysisEntries;
};

}

#endif // RIBBONENTRY_H
