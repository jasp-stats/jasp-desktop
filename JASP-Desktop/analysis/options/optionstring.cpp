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

#include "optionstring.h"
#include "utils.h"

using namespace std;

void OptionString::init(const Json::Value &data)
{
	_value	= data.get("default", "").asString();
	_regexp = data.get("regexp", "").asString();
	_max	= data.get("max", -1).asInt();
}

Json::Value OptionString::asJSON() const
{
	return _value;
}

void OptionString::set(const Json::Value &value)
{
	if (value.isInt() || value.isUInt() || value.isDouble())
		// Why the json library cannot convert it to string???
		setValue(Utils::doubleToString(value.asDouble()));
	else
		setValue(value.asString());
}

Option *OptionString::clone() const
{
	OptionString *c = new OptionString();
	c->setValue(_value);
	return c;
}

int OptionString::max() const
{
	return _max;
}

string OptionString::regexp() const
{
	return _regexp;
}
