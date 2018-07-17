#ifndef ANALYSISENTRY_H
#define ANALYSISENTRY_H

#include <string>
#include <vector>
#include "jsonredirect.h"

namespace Modules
{

class AnalysisEntry
{
public:
	AnalysisEntry(Json::Value & analysisEntry) :
		_title(				analysisEntry.get("title",				"???").asString()),
		_function(			analysisEntry.get("function",			"???").asString()),
		_qml(				analysisEntry.get("qml",				"???").asString()),
		_usesJaspResults(	analysisEntry.get("usesJaspResults",	true).asBool())
	{}

	std::string	title()				const { return _title;				}
	std::string	function()			const { return _function;			}
	std::string	qml()				const { return _qml;				}
	bool		usesJaspResults()	const { return _usesJaspResults;	}

private:
	std::string		_title,
					_function,
					_qml;
	bool			_usesJaspResults;
};

}

#endif // ANALYSISENTRY_H
