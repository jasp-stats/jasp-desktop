
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
		BOOST_FOREACH(string field, _value.front())
			v.append(field);
	}

	return v;
}

void OptionVariables::set(Json::Value &value)
{
	vector<string> terms;

	if (value.isArray())
	{
		for (uint i = 0; i < value.size(); i++)
			terms.push_back(value[i].asString());
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
	if (_value.size() > 0)
		return _value.front();
	else
		return vector<string>();
}

