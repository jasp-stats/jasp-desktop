#include "optionboolean.h"

OptionBoolean::OptionBoolean(bool defaultValue)
	: OptionI()
{
	_value = defaultValue;
}

void OptionBoolean::init(const Json::Value &data)
{
	_value = data.get("default", false).asBool();
}

Json::Value OptionBoolean::asJSON() const
{
	return Json::Value(value());
}

void OptionBoolean::set(const Json::Value &value)
{
	setValue(value.asBool());
}

Option *OptionBoolean::clone() const
{
	return new OptionBoolean(_value);
}
