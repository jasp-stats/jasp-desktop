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
	OptionsTable(Options *rowTemplate = NULL) : OptionI(true), _template(rowTemplate) {}

	void		init(const Json::Value &data)							override;
	void		set(const Json::Value &value)							override;
	Json::Value asJSON()										const	override;
	Option*		clone()											const	override;
	void		setValue(const std::vector<Options *> &value)			override;
	void		connectOptions(const std::vector<Options *> &value);

	Options*	rowTemplate() const;

private:
	Options *_template;
	
	void optionsChanged(Option *);	
};

#endif // OPTIONSTABLE_H
