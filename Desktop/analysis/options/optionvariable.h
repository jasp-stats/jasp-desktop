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

#ifndef OPTIONVARIABLE_H
#define OPTIONVARIABLE_H

#include <string>
#include "optionvariables.h"
#include "common.h"
#include "jsonredirect.h"

class OptionVariable : public OptionVariables
{
public:
	OptionVariable() : OptionVariables(true, false) { setContainsColumn(true);	}

	void					set(const Json::Value& value)			override;
	Json::Value				asJSON()						const	override;
	Option					*clone()						const	override;

	std::string				variable()						const;
	std::set<std::string>	usedVariables()					const	override { return variable() == "" ? std::set<std::string>({}) : std::set<std::string>({ variable() }); }
	void					removeUsedVariable(const std::string & var)		override;

};

#endif // OPTIONVARIABLE_H
