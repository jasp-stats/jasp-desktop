#include "optioninteger.h"

OptionInteger::OptionInteger(int value)
	: OptionI()
{
	_value = value;
}

Json::Value OptionInteger::asJSON() const
{
	return Json::Value(_value);
}

void OptionInteger::set(Json::Value &value)
{
	_value = value.asInt();
}

Option *OptionInteger::clone() const
{
	return new OptionInteger(value());
}
