#include "optionintegerarray.h"

#include <boost/foreach.hpp>

using namespace std;

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
	vector<int> ints;

	for (Json::ValueIterator itr = value.begin(); itr != value.end(); itr++)
		ints.push_back((*itr).asInt());

	_value = ints;

}
