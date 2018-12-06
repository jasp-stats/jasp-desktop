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
	RibbonEntry(Json::Value & ribbonEntry, DynamicModule * parentDynamicModule);
	~RibbonEntry();

	std::string				title()				const	{ return _title;			}
	std::string				icon()				const;
	const AnalysisEntries&	analysisEntries()	const	{ return _analysisEntries;	}
	DynamicModule*			dynamicModule()		const	{ return _dynamicModule;	}

	AnalysisEntry*			retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile);
	AnalysisEntry*			analysisEntry(const std::string & analysisTitle) const;
	AnalysisEntry*			operator[](const std::string & analysisTitle) const { return analysisEntry(analysisTitle); }

private:
	std::string				_title,
							_icon;

	AnalysisEntries			_analysisEntries;
	DynamicModule			*_dynamicModule = nullptr;
};

typedef std::vector<RibbonEntry*>	RibbonEntries;

}

#endif // RIBBONENTRY_H
