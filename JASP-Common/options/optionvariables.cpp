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

#include "optionvariables.h"

#include <boost/foreach.hpp>
#include "common.h"

using namespace std;

OptionVariables::OptionVariables()
	: OptionTerms(true, false)
{
}

OptionVariables::OptionVariables(bool onlyOneTerm)
	: OptionTerms(true, onlyOneTerm)
{
}

Json::Value OptionVariables::asJSON() const
{
	Json::Value v(Json::arrayValue);

	if (_value.size() > 0)
	{
		BOOST_FOREACH(vector<string> variable, _value)
			v.append(variable.front());
	}

	return v;
}

void OptionVariables::set(const Json::Value &value)
{
	vector<string> terms;

	if (value.isArray())
	{
		for (uint i = 0; i < value.size(); i++)
		{
			string v = value[i].asString();
			terms.push_back(v);
		}
	}

	setValue(terms);
}

Option *OptionVariables::clone() const
{
	OptionVariables *c = new OptionVariables();
	c->setValue(value());
	return c;
}

vector<string> OptionVariables::variables() const
{
	vector<string> values;

	for (size_t i = 0; i < _value.size(); i++)
	{
		const vector<string> &variable = _value.at(i);
		if (variable.size() > 0)
			values.push_back(variable.at(0));
	}

	return values;
}

void OptionVariables::replaceName(string oldName, string newName)
{
	vector<string> values = variables();
	replace(values.begin(), values.end(), oldName, newName);
	setValue(values);
}

void OptionVariables::removeName(string name)
{
	vector<string> values = variables();
	values.erase(remove(values.begin(), values.end(), name), values.end());
	setValue(values);
}
