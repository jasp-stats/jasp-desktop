#include "optionlist.h"

OptionList::OptionList(std::string name, std::string selected)
	: OptionI(name)
{
	_value = selected;
}

Json::Value OptionList::asJSON() const
{
	return _value;
}

void OptionList::set(Json::Value &value)
{
}
