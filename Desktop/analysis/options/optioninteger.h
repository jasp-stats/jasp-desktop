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

#ifndef OPTIONINTEGER_H
#define OPTIONINTEGER_H

#include "optioni.h"
#include "common.h"

#include <climits>
#include <string>

class OptionInteger : public OptionI<int>
{
public:
	OptionInteger(int value = 0, int minimum = -999999, int maximum = 999999, std::string format = "") : OptionI(value), _min(minimum), _max(maximum), _format(format) {}

	void		init(const Json::Value &data)			override;
	void		set(const Json::Value& value)			override;
	Json::Value asJSON()						const	override;
	Option		*clone()						const	override;

	int minimum() const;
	int maximum() const;

	std::string format() const;

protected:
	int			_min;
	int			_max;
	std::string _format;
};

#endif // OPTIONINTEGER_H
