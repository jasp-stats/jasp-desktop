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

#ifndef OPTIONVARIABLES_H
#define OPTIONVARIABLES_H

#include "optionterms.h"
#include "optionvariablei.h"

class OptionVariables : public OptionTerms, public OptionVariableI
{
public:
	OptionVariables(bool extraEncodes = false)									: OptionTerms(true, false), _extraEncodings(extraEncodes) {}
	OptionVariables(std::vector<std::string> vars, bool extraEncodes = false)	: OptionTerms(true, false), _extraEncodings(extraEncodes) { setValue(vars); }

	Json::Value					asJSON()												const	override;
	Json::Value					asMetaJSON()											const	override;
	void						set(const Json::Value& value)									override;
	Option*						clone()													const	override;

	std::vector<std::string>	variables()												const	override;
	void						replaceName(std::string oldName, std::string newName)			override;
	void						removeName(std::string name)									override;

	std::set<std::string>		usedVariables()											const	override;
	void						removeUsedVariable(const std::string & var)						override;
	void						replaceVariableName(const std::string & oldName, const std::string & newName)	override	{ replaceName(oldName, newName); }

	void						setExtraEncodings(bool theyAreIt)											{ _extraEncodings = theyAreIt; }

protected:
	OptionVariables(bool onlyOneTerm, bool extraEncodes) : OptionTerms(true, onlyOneTerm), _extraEncodings(extraEncodes)	{}

	bool _extraEncodings = false;
};

#endif // OPTIONVARIABLES_H
