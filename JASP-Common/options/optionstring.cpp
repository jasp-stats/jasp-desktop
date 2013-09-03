#include "optionstring.h"

using namespace std;

OptionString::OptionString(string name)
	: OptionI(name)
{
}

Json::Value OptionString::asJSON() const
{
	return _value;
}

void OptionString::set(Json::Value &value)
{
	setValue(value.asString());
}
