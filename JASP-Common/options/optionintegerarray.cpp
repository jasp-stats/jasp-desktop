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

#include "optionintegerarray.h"

#include <boost/foreach.hpp>

using namespace std;

OptionIntegerArray::OptionIntegerArray()
{
}

Json::Value OptionIntegerArray::asJSON() const
{
	Json::Value array = Json::arrayValue;

	BOOST_FOREACH(int value, _value)
		array.append(value);

	return array;
}

void OptionIntegerArray::set(const Json::Value &value)
{	
	vector<int> ints;

	for (Json::ValueIterator itr = value.begin(); itr != value.end(); itr++)
		ints.push_back((*itr).asInt());

	_value = ints;

}

Option *OptionIntegerArray::clone() const
{
	OptionIntegerArray *c = new OptionIntegerArray();
	c->setValue(value());
	return c;
}
