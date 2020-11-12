//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef OPTIONVARIABLESGROUPS_H
#define OPTIONVARIABLESGROUPS_H

#include "optioni.h"
#include "optionvariablei.h"

#include "common.h"

class OptionVariablesGroups : public OptionI<std::vector<std::vector<std::string> > >, public OptionVariableI
{
public:
	OptionVariablesGroups() : OptionI(true) { setShouldEncode(true); }

	Json::Value					asJSON()												const	override;
	void						set(const Json::Value& value)									override;
	Option						*clone()												const	override;

	std::vector<std::string>	variables()												const	override;
	void						replaceName(std::string oldName, std::string newName)			override;
	void						removeName(std::string name)									override;
	std::set<std::string>		usedVariables()											const	override;
	void						removeUsedVariable(const std::string & var)										override	{ removeName(var); }
	void						replaceVariableName(const std::string & oldName, const std::string & newName)	override	{ replaceName(oldName, newName); }
};

#endif // OPTIONVARIABLESGROUPS_H
