#include "optionboolean.h"

OptionBoolean::OptionBoolean(std::string name)
	: OptionI(name)
{
	_value = false;
}

Json::Value OptionBoolean::asJSON() const
{
	return Json::Value(value());
}
