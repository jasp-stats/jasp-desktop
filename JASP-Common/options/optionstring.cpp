#include "optionstring.h"

using namespace std;

OptionString::OptionString(string value)
{
	_value = value;
}

Json::Value OptionString::asJSON() const
{
	return _value;
}

void OptionString::set(Json::Value &value)
{
	setValue(value.asString());
}

Option *OptionString::clone() const
{
	OptionString *c = new OptionString();
	c->setValue(_value);
	return c;
}
