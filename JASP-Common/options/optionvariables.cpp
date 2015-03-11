
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
	vector<string> variables;

	for (int i = 0; i < _value.size(); i++)
	{
		const vector<string> &variable = _value.at(i);
		if (variable.size() > 0)
			variables.push_back(variable.at(0));
	}

	return variables;
}

