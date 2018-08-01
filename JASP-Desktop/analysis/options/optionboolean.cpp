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

#include "optionboolean.h"

void OptionBoolean::init(const Json::Value &data)
{
	_value = data.get("default", false).asBool();
}

Json::Value OptionBoolean::asJSON() const
{
	return Json::Value(value());
}

void OptionBoolean::set(const Json::Value &value)
{
	setValue(value.asBool());
}

Option *OptionBoolean::clone() const
{
	return new OptionBoolean(_value);
}
