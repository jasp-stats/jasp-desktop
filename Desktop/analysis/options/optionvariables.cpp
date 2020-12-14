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

#include "optionvariables.h"


#include "common.h"

using namespace std;



Json::Value OptionVariables::asJSON() const
{
	Json::Value v(Json::arrayValue);

	for(vector<string> variable : _value)
		v.append(variable.front());

	return v;
}

Json::Value OptionVariables::asMetaJSON() const
{
	Json::Value metaBase = defaultMetaEntryContainingColumn(_extraEncodings || _shouldEncode);

	if(_extraEncodings)
		metaBase["encodeThis"] = asJSON();

	return metaBase;
}

void OptionVariables::set(const Json::Value &value)
{
	vector<string> terms;

	if (value.isArray())
		for (uint i = 0; i < value.size(); i++)
			terms.push_back(value[i].asString());

	setValue(terms);
}

Option *OptionVariables::clone() const
{
	OptionVariables *c = new OptionVariables(_extraEncodings);
	c->setValue(value());
	c->setShouldEncode(_shouldEncode); // Should maybe be more generic then this.. But rewriting and removing all of these damned optionclasses ought to help
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

std::set<std::string> OptionVariables::usedVariables() const
{
	std::set<std::string> values;

	for (size_t i = 0; i < _value.size(); i++)
	{
		const vector<string> &variable = _value.at(i);
		if (variable.size() > 0)
			values.insert(variable.at(0));
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

