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

