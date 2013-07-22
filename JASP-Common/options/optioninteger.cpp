#include "optioninteger.h"

OptionInteger::OptionInteger(std::string name, int value)
	: OptionI(name)
{
	_value = value;
}

Json::Value OptionInteger::asJSON() const
{
	return Json::Value(_value);
}

void OptionInteger::set(Json::Value &value)
{
}
