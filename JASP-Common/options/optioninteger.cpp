#include "optioninteger.h"

OptionInteger::OptionInteger(int value)
	: OptionI()
{
	_value = value;
}

void OptionInteger::init(const Json::Value &data)
{
	_value = data.get("default", 0).asInt();
}

Json::Value OptionInteger::asJSON() const
{
	return Json::Value(_value);
}

void OptionInteger::set(const Json::Value &value)
{
	_value = value.asInt();
}

Option *OptionInteger::clone() const
{
	return new OptionInteger(value());
}
