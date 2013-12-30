#include "optionfields.h"

#include <boost/foreach.hpp>

using namespace std;

OptionFields::OptionFields()
	: OptionI()
{
}

Json::Value OptionFields::asJSON() const
{
	Json::Value v(Json::arrayValue);

	BOOST_FOREACH(string field, _value)
	{
		v.append(field);
	}

	return v;
}

void OptionFields::set(Json::Value &value)
{
	vector<string> fields;

	if (value.isArray())
	{
		for (int i = 0; i < value.size(); i++)
			fields.push_back(value[i].asString());
	}

	setValue(fields);
}

Option *OptionFields::clone() const
{
	OptionFields *c = new OptionFields();
	c->setValue(value());
	return c;
}

void OptionFields::setRestrictions(int restrictions)
{
}

