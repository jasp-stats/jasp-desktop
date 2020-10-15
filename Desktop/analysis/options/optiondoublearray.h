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

#ifndef OPTIONDOUBLEARRAY_H
#define OPTIONDOUBLEARRAY_H

#include "optioni.h"
#include "common.h"

class OptionDoubleArray : public OptionI<std::vector<double> >
{
public:
	OptionDoubleArray()									: OptionI() {}
	OptionDoubleArray(const std::vector<double> & vals) : OptionI(vals) {}

	void		init(const Json::Value &data)			override;
	void		set(const Json::Value& value)			override;
	Json::Value asJSON()						const	override;
	Option		*clone()						const	override;
};

#endif // OPTIONDOUBLEARRAY_H
