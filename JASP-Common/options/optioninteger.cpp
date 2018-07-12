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

#include "optioninteger.h"

void OptionInteger::init(const Json::Value &data)
{
	_value	= data.get("default", 0).asInt();
	_min	= data.get("min", -999999).asInt();
	_max	= data.get("max",  999999).asInt();
	_format = data.get("format", "").asString();
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

int OptionInteger::minimum() const
{
  return _min;
}

int OptionInteger::maximum() const
{
  return _max;
}

std::string OptionInteger::format() const
{
  return _format;
}

