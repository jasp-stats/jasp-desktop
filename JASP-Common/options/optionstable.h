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

#ifndef OPTIONSTABLE_H
#define OPTIONSTABLE_H

#include "optioni.h"
#include "options.h"

#include "common.h"
#include "option.h"
#include "options.h"

class OptionsTable : public OptionI<std::vector<Options*> >
{
public:
	OptionsTable(Options *rowTemplate = NULL);

	virtual void init(const Json::Value &data) OVERRIDE;

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(const Json::Value &value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;
	virtual void setValue(const std::vector<Options *> &value) OVERRIDE;

	Options *rowTemplate() const;

private:
	Options *_template;
};

#endif // OPTIONSTABLE_H
