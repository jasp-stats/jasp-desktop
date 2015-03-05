#include "optionlist.h"
#include <boost/foreach.hpp>

OptionList::OptionList(const std::vector<std::string> &options, std::string selected)
{
	_options = options;
	if (selected == "")
		_value = options.at(0);
	else
		_value = selected;
}

OptionList::OptionList()
{

}

void OptionList::loadData(const Json::Value &data)
{
	_options = std::vector<std::string>();

	const Json::Value &array = data["options"];
	for (Json::ValueIterator itr = array.begin(); itr != array.end(); itr++)
		_options.push_back((*itr).asString());

	if (data.isMember("default"))
		_value = data["default"].asString();
	else
		_value = _options.at(0);
}

Json::Value OptionList::asJSON() const
{
	return _value;
}

void OptionList::set(const Json::Value &value)
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
