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

#include "optionnumber.h"

using namespace std;

OptionNumber::OptionNumber(double value, double min, double max, string format)
{
	_value = value;
	_min = min;
	_max = max;
	_format = format;
}

OptionNumber::OptionNumber()
{
}

void OptionNumber::init(const Json::Value &data)
{
	_value = data.get("value", 0.0).asDouble();
	_min = data.get("min", -999999.0).asDouble();
	_max = data.get("max",  999999.0).asDouble();
	_format = data.get("format", "").asString();
}

Json::Value OptionNumber::asJSON() const
{
	return Json::Value(_value);
}

void OptionNumber::set(const Json::Value &value)
{
	_value = value.asDouble();
}

Option *OptionNumber::clone() const
{
	return new OptionNumber(_value, _min, _max, _format);
}

double OptionNumber::min() const
{
	return _min;
}

double OptionNumber::max() const
{
	return _max;
}

string OptionNumber::format() const
{
	return _format;
}
