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

#include "optionvariablesgroups.h"



using namespace std;

Json::Value OptionVariablesGroups::asJSON() const
{
	Json::Value v = Json::arrayValue;

	for(const vector<string> &group : _value)
	{
		Json::Value u = Json::arrayValue;

		for(const string &variable : group)
			u.append(variable);

		v.append(u);
	}

	return v;
}

Json::Value OptionVariablesGroups::asMetaJSON() const
{
	return defaultMetaEntryContainingColumn();
}

void OptionVariablesGroups::set(const Json::Value &value)
{
	vector<vector<string> > groups;

	for (auto &g : value)
	{
		vector<string> group;

		for (auto &v : g)
			group.push_back(v.asString());

		groups.push_back(group);
	}

	setValue(groups);
}

Option *OptionVariablesGroups::clone() const
{
	OptionVariablesGroups *c = new OptionVariablesGroups();
	c->setValue(value());
	return c;
}

std::vector<std::string> OptionVariablesGroups::variables() const
{
	std::vector<std::string> variables;
	vector<vector<string> > values = value();

	for (vector<vector<string> >::iterator it = values.begin(); it != values.end(); ++it)
		variables.insert(variables.end(), it->begin(), it->end());

	return variables;
}

std::set<std::string> OptionVariablesGroups::usedVariables() const
{
	std::set<std::string>					variables;
	std::vector<std::vector<std::string>>	values = value();

	for (vector<vector<string> >::iterator it = values.begin(); it != values.end(); ++it)
		variables.insert(it->begin(), it->end());

	return variables;
}

void OptionVariablesGroups::replaceName(string oldName, string newName)
{
	vector<vector<string> > values = value();
	vector<vector<string> > values_updated;
	for (vector<vector<string> >::iterator values_it = values.begin(); values_it != values.end(); ++values_it)
	{
		vector<string> row = *values_it;
		replace(row.begin(), row.end(), oldName, newName);
		values_updated.push_back(row);
	}
	setValue(values_updated);
}

void OptionVariablesGroups::removeName(string name)
{
	vector<vector<string> > values = value();
	vector<vector<string> > values_updated;
	for (vector<vector<string> >::iterator values_it = values.begin(); values_it != values.end(); ++values_it)
	{
		vector<string> row = *values_it;
		if (std::find(row.begin(), row.end(), name) == row.end())
			values_updated.push_back(row);
	}
	setValue(values_updated);

}


