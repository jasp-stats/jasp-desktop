//
// Copyright (C) 2013-2016 University of Amsterdam
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

#include "optioninteger.h"

OptionInteger::OptionInteger(int value)
	: OptionI()
{
	_value = value;
}

void OptionInteger::init(const Json::Value &data)
{
	_value = data.get("default", 0).asInt();
}

Json::Value OptionInteger::asJSON() const
{
	return Json::Value(_value);
}

void OptionInteger::set(const Json::Value &value)
{
	_value = value.asInt();
}

Option *OptionInteger::clone() const
{
	return new OptionInteger(value());
}
