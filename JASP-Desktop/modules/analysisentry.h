#ifndef ANALYSISENTRY_H
#define ANALYSISENTRY_H

#include <string>
#include <vector>
#include "jsonredirect.h"

namespace Modules
{
class RibbonEntry;
class DynamicModule;

class AnalysisEntry
{
public:
	AnalysisEntry(Json::Value & analysisEntry, RibbonEntry * parentRibbonEntry) :
		_title(				analysisEntry.get("title",				"???").asString()),
		_function(			analysisEntry.get("function",			"???").asString()),
		_qml(				analysisEntry.get("qml",				"???").asString()),
		_ribbonEntry(		parentRibbonEntry)
	{}

	std::string		title()					const { return _title;				}
	std::string		function()				const { return _function;			}
	std::string		qml()					const { return _qml;				}

	RibbonEntry*	ribbonEntry()			const { return _ribbonEntry;		}
	DynamicModule*	dynamicModule()			const;
	std::string		qmlFilePath()			const;
	std::string		getFullRCall()			const;
	Json::Value		getDefaultResults()		const;
	Json::Value		asJsonForJaspFile()	const;

private:
	std::string		_title		= "",
					_function	= "",
					_qml		= "";

	RibbonEntry*	_ribbonEntry = NULL;
};

}

#endif // ANALYSISENTRY_H
