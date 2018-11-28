//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//


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
