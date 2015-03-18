
#include "optionterm.h"

#include <boost/foreach.hpp>

using namespace std;

OptionTerm::OptionTerm()
	: OptionTerms(false, true)
{
}

Json::Value OptionTerm::asJSON() const
{
	Json::Value v(Json::arrayValue);

	if (_value.size() > 0)
	{
		BOOST_FOREACH(string field, _value.front())
		{
			v.append(field);
		}
	}

	return v;
}

void OptionTerm::set(const Json::Value &value)
{
	vector<string> terms;

	if (value.isArray())
	{
		for (uint i = 0; i < value.size(); i++)
			terms.push_back(value[i].asString());
	}

	setValue(terms);
}

Option *OptionTerm::clone() const
{
	OptionTerm *c = new OptionTerm();
	c->setValue(this->term());
	return c;
}

void OptionTerm::setValue(const vector<string> &value)
{
	vector<vector<string> > terms;
	terms.push_back(value);
	OptionTerms::setValue(terms);
}

vector<string> OptionTerm::term() const
{
	if (_value.size() > 0)
		return _value.front();
	else
		return vector<string>();
}
