#include "optionintegerarray.h"

#include <boost/foreach.hpp>

OptionIntegerArray::OptionIntegerArray(std::string name)
	: OptionI(name)
{
}

Json::Value OptionIntegerArray::asJSON() const
{
	Json::Value array = Json::arrayValue;

	BOOST_FOREACH(int value, _value)
			array.append(value);

	return array;
}

void OptionIntegerArray::set(Json::Value &value)
{
}
