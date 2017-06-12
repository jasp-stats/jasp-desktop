//
// Copyright (C) 2013-2017 University of Amsterdam
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
	OptionVariables();

	virtual Json::Value asJSON()const OVERRIDE;
	virtual void set(const Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	virtual std::vector<std::string> variables() const OVERRIDE;
	virtual void replaceName(std::string oldName, std::string newName) OVERRIDE;
	virtual void removeName(std::string name) OVERRIDE;

protected:
	OptionVariables(bool onlyOneTerm);

};

#endif // OPTIONVARIABLES_H
