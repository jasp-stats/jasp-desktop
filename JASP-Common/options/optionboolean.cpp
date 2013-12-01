#include "optionboolean.h"

OptionBoolean::OptionBoolean(std::string name, bool defaultValue)
	: OptionI(name)
{
	_value = defaultValue;
}

Json::Value OptionBoolean::asJSON() const
{
	return Json::Value(value());
}

void OptionBoolean::set(Json::Value &value)
{
	setValue(value.asBool());
}
