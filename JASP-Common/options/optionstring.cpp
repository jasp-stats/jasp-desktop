#include "optionstring.h"

using namespace std;

OptionString::OptionString(string value)
{
	_value = value;
}

void OptionString::init(const Json::Value &data)
{
	_value = data.get("default", "").asString();
}

Json::Value OptionString::asJSON() const
{
	return _value;
}

void OptionString::set(const Json::Value &value)
{
	setValue(value.asString());
}

Option *OptionString::clone() const
{
	OptionString *c = new OptionString();
	c->setValue(_value);
	return c;
}
