#include "optionboolean.h"

OptionBoolean::OptionBoolean(bool defaultValue)
	: OptionI()
{
	_value = defaultValue;
}

void OptionBoolean::loadData(Json::Value data)
{
	_value = data["default"].asBool();
}

Json::Value OptionBoolean::asJSON() const
{
	return Json::Value(value());
}

void OptionBoolean::set(Json::Value &value)
{
	setValue(value.asBool());
}

Option *OptionBoolean::clone() const
{
	return new OptionBoolean(_value);
}
