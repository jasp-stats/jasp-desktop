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

#include "optionterms.h"


using namespace std;


void OptionTerms::set(const Json::Value &value)
{
	if ( ! value.isArray())
		return;

	vector<vector<string> > terms;

	for (size_t i = 0; i < value.size(); i++)
	{
		const Json::Value &jVariable = value.get(i, Json::nullValue);
		if ( ! jVariable.isArray())
			return;

		vector<string> term;

		for (size_t j = 0; j < jVariable.size(); j++)
		{
			Json::Value jComponent = jVariable.get(j, Json::nullValue);
			if ( ! jComponent.isString())
				return;

			term.push_back(jComponent.asString());
		}

		terms.push_back(term);
	}

	setValue(terms);
}

Json::Value OptionTerms::asJSON() const
{
	Json::Value top;

	for(const vector<string> &term : _value)
	{
		Json::Value row(Json::arrayValue);

		for(const string &component : term)
		{
			row.append(component);
		}

		top.append(row);
	}

	return top;
}

Option *OptionTerms::clone() const
{
	OptionTerms *c = new OptionTerms(_onlyOneComponent, _onlyOneTerm);
	c->setValue(value());
	c->setShouldEncode(_shouldEncode);
	return c;
}

void OptionTerms::init(const Json::Value &data)
{
	Json::Value def4ult = data.get("default", Json::nullValue);
	if (def4ult.isNull() == false)
		set(def4ult);
}

void OptionTerms::setValue(const vector<vector<string> > &value)
{
	vector<vector<string> > v = value;

	if (_onlyOneTerm && value.size() > 1)
	{
		v.erase(++v.begin(), v.end());

		if (_onlyOneComponent && _onlyOneTerm && v.front().size() > 1)
		{
			vector<string> &term = v.front();
			term.erase(++term.begin(), term.end());
		}
	}

	OptionI::setValue(value);
}

void OptionTerms::setValue(const vector<string> &value)
{
	vector<vector<string> > terms;

	for(string variable : value)
	{
		vector<string> components;
		components.push_back(variable);
		terms.push_back(components);
	}

	setValue(terms);
}

void OptionTerms::setValue(const string &value)
{
	vector<string> term;
	vector<vector<string> > terms;

	term.push_back(value);
	terms.push_back(term);

	setValue(terms);
}

bool OptionTerms::onlyOneTerm() const
{
	return _onlyOneTerm;
}

bool OptionTerms::onlyOneComponent() const
{
	return _onlyOneComponent;
}
