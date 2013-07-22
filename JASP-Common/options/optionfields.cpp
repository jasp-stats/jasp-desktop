#include "optionfields.h"

#include <boost/foreach.hpp>

using namespace std;

OptionFields::OptionFields(string name)
	: OptionI(name)
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

void OptionFields::setRestrictions(int restrictions)
{
}

