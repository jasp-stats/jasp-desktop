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

#include "optiondoublearray.h"



using namespace std;

void OptionDoubleArray::init(const Json::Value &data)
{
	this->set(data.get("default", Json::nullValue));
}

Json::Value OptionDoubleArray::asJSON() const
{
	Json::Value array = Json::arrayValue;

	for(double value : _value)
		array.append(value);

	return array;
}

void OptionDoubleArray::set(const Json::Value &value)
{
	vector<double> dbls;

	for (auto itr = value.begin(); itr != value.end(); itr++)
		dbls.push_back((*itr).asDouble());

	_value = dbls;
}

Option *OptionDoubleArray::clone() const
{
	OptionDoubleArray *c = new OptionDoubleArray();
	c->setValue(value());
	return c;
}
