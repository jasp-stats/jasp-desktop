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

#include "optionstring.h"

using namespace std;

OptionString::OptionString(string value)
{
	_value = value;
}

void OptionString::init(const Json::Value &data)
{
	_value = data.get("default", "").asString();
}

Json::Value OptionString::asJSON() const
{
	return _value;
}

void OptionString::set(const Json::Value &value)
{
	setValue(value.asString());
}

Option *OptionString::clone() const
{
	OptionString *c = new OptionString();
	c->setValue(_value);
	return c;
}
