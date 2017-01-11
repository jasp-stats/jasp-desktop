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

#ifndef OPTIONTERMS_H
#define OPTIONTERMS_H

#include "optioni.h"
#include <list>
#include "../common.h"

class OptionTerms : public OptionI<std::vector<std::vector<std::string> > >
{
public:
	OptionTerms();

	virtual void set(const Json::Value& value) OVERRIDE;
	virtual Json::Value asJSON() const OVERRIDE;
	virtual Option* clone() const OVERRIDE;
	virtual void init(const Json::Value &data) OVERRIDE;

	virtual void setValue(const std::vector<std::vector<std::string> > &value) OVERRIDE;
	virtual void setValue(const std::vector<std::string> &value);
	virtual void setValue(const std::string &value);

	bool onlyOneTerm() const;
	bool onlyOneComponent() const;

protected:
	OptionTerms(bool onlyOneComponent, bool onlyOneTerm = false);

	bool _onlyOneComponent;
	bool _onlyOneTerm;
};

#endif // OPTIONTERMS_H
