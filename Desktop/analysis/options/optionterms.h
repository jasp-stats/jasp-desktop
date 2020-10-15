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

#ifndef OPTIONTERMS_H
#define OPTIONTERMS_H

#include "optioni.h"
#include <list>
#include "common.h"

class OptionTerms : public OptionI<std::vector<std::vector<std::string> > >
{
public:
			OptionTerms() : OptionI(true), _onlyOneComponent(false), _onlyOneTerm(false) {}

			void		set(const Json::Value& value) override;
			Json::Value asJSON() const override;
			Option		*clone() const override;
			void		init(const Json::Value &data) override;

			void		setValue(const std::vector<std::vector<std::string> > &value) override;
	virtual void		setValue(const std::vector<std::string> &value);
	virtual void		setValue(const std::string &value);

	bool				onlyOneTerm() const;
	bool				onlyOneComponent() const;

protected:
	OptionTerms(bool onlyOneComponent, bool onlyOneTerm = false) : OptionI(true), _onlyOneComponent(onlyOneComponent), _onlyOneTerm(onlyOneTerm) {}

	bool _onlyOneComponent;
	bool _onlyOneTerm;
};

#endif // OPTIONTERMS_H
