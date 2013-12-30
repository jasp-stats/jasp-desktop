#include "optionlist.h"

OptionList::OptionList(const std::vector<std::string> &options, std::string selected)
{
	_options = options;
	if (selected == "")
		_value = options.at(0);
	else
		_value = selected;
}

Json::Value OptionList::asJSON() const
{
	return _value;
}

void OptionList::set(Json::Value &value)
{
	_value = value.asString();
}

void OptionList::set(int index)
{
	if (index < _options.size())
	{
		setValue(_options[index]);
	}
}

const std::vector<std::string> OptionList::options() const
{
	return _options;
}

Option *OptionList::clone() const
{
	return new OptionList(_options, _value);
}
