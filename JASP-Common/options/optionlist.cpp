//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

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

void OptionList::init(const Json::Value &data)
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

void OptionList::set(size_t index)
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
