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

#include "options.h"

#include <boost/foreach.hpp>

#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionstable.h"
#include "options/optionstring.h"
#include "options/optionterm.h"
#include "options/optionterms.h"
#include "options/optionvariable.h"
#include "options/optionvariables.h"
#include "options/optionvariablesgroups.h"

using namespace std;

Options::Options()
	: names(&_options)
{
}

Options::~Options()
{
	BOOST_FOREACH(OptionNamed item, _options)
		delete item.second;
}

Json::Value Options::asJSON() const
{
	return asJSON(true);
}

void Options::init(const Json::Value &array)
{
	for (Json::ValueIterator itr = array.begin(); itr != array.end(); itr++)
	{
		Json::Value value = (*itr);

		Json::Value &name = value["name"];
		Json::Value &type = value["type"];

		string typeString = type.asString();
		Option *option = createOption(typeString);

		if (option != NULL)
		{
			option->init(value);
			add(name.asString(), option);
		}
		else
		{
			cout << "Unknown data type: " << typeString << "\n";
			cout.flush();
		}
	}
}

Option* Options::createOption(string typeString)
{
	if (typeString == "Boolean")
		return new OptionBoolean();
	else if (typeString == "Integer")
		return new OptionInteger();
	else if (typeString == "IntegerArray")
		return new OptionIntegerArray();
	else if (typeString == "List")
		return new OptionList();
	else if (typeString == "Number")
		return new OptionNumber();
	else if (typeString == "Table")
		return new OptionsTable();
	else if (typeString == "String")
		return new OptionString();
	else if (typeString == "Term")
		return new OptionTerm();
	else if (typeString == "Terms")
		return new OptionTerms();
	else if (typeString == "Variable")
		return new OptionVariable();
	else if (typeString == "Variables")
		return new OptionVariables();
	else if (typeString == "VariablesGroups")
		return new OptionVariablesGroups();

	return NULL;
}

void Options::add(string name, Option *option)
{
	_options.push_back(OptionNamed(name, option));
	option->changed.connect(boost::bind(&Options::optionsChanged, this, _1));
}

void Options::optionsChanged(Option *option)
{
	notifyChanged();
}

Json::Value Options::asJSON(bool includeTransient) const
{
	Json::Value top = Json::objectValue;

	BOOST_FOREACH(OptionNamed item, _options)
	{
		if (includeTransient == false && item.second->isTransient())
			continue;

		string name = item.first;
		Json::Value value = item.second->asJSON();
		insertValue(name, value, top);
	}

	return top;
}

void Options::set(const Json::Value &json)
{
	BOOST_FOREACH(OptionNamed item, _options)
	{
		string name = item.first;
		Json::Value value;
		if (extractValue(name, json, value))
			item.second->set(value);
	}

	optionsChanged(this);
}

void Options::insertValue(const string &name, Json::Value &value, Json::Value &root)
{
	size_t endPos;

	if ((endPos = name.find('/', 0)) != string::npos)
	{
		string groupName = name.substr(0, endPos);
		string itemName = name.substr(endPos + 1);

		insertValue(itemName, value, root[groupName]);
	}
	else
	{
		root[name] = value;
	}
}

bool Options::extractValue(const string &name, const Json::Value &root, Json::Value &value)
{
	size_t endPos;

	if ((endPos = name.find('/', 0)) != string::npos)
	{
		string groupName = name.substr(0, endPos);
		string itemName = name.substr(endPos + 1);

		return extractValue(itemName, root[groupName], value);
	}
	else if (root.isMember(name))
	{
		value = root[name];
		return true;
	}
	else
	{
		return false;
	}
}

Option *Options::get(string name) const
{
	BOOST_FOREACH(OptionNamed p, _options)
	{
		if (p.first == name)
			return p.second;
	}

	return NULL;
}

Option *Options::get(int index)
{
	return _options.at(index).second;
}

void Options::get(int index, string &name, Option *&option)
{
	OptionNamed optionWithName = _options.at(index);
	name = optionWithName.first;
	option = optionWithName.second;
}

Option *Options::clone() const
{
	Options *c = new Options();

	BOOST_FOREACH(const OptionNamed &option, _options)
	{
		(void)_options;
		Option *oc = option.second->clone();
		c->add(option.first, oc);
	}

	return c;
}

size_t Options::size() const
{
	return _options.size();
}

