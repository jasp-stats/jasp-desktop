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
