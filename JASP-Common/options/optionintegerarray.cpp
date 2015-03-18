#include "optionintegerarray.h"

#include <boost/foreach.hpp>

using namespace std;

OptionIntegerArray::OptionIntegerArray()
{
}

Json::Value OptionIntegerArray::asJSON() const
{
	Json::Value array = Json::arrayValue;

	BOOST_FOREACH(int value, _value)
		array.append(value);

	return array;
}

void OptionIntegerArray::set(const Json::Value &value)
{	
	vector<int> ints;

	for (Json::ValueIterator itr = value.begin(); itr != value.end(); itr++)
		ints.push_back((*itr).asInt());

	_value = ints;

}

Option *OptionIntegerArray::clone() const
{
	OptionIntegerArray *c = new OptionIntegerArray();
	c->setValue(value());
	return c;
}
