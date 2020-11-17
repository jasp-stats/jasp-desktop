//
// Copyright (C) 2013-2018 University of Amsterdam
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
#include "optionlist.h"
#include "optionterm.h"
#include "optionterms.h"
#include "optionnumber.h"
#include "optionstable.h"
#include "optionstring.h"
#include "optionboolean.h"
#include "optioninteger.h"
#include "optionvariable.h"
#include "optionvariables.h"
#include "optiondoublearray.h"
#include "optionintegerarray.h"
#include "optioncomputedcolumn.h"
#include "optionvariablesgroups.h"

using namespace std;


Options::~Options()
{
	for(OptionNamed item : _options)
		delete item.second;
}

Json::Value Options::asJSON() const
{
	return asJSON(true);
}

Json::Value Options::asJSONWithMeta() const
{
	Json::Value json			= asJSON(true);
				json[".meta"]	= asMetaJSON();

	return json;
}

void Options::add(string name, Option *option)
{
	remove(name);
	_options.push_back(OptionNamed(name, option));
	option->changed.connect(							boost::bind( &Options::optionsChanged,							this, _1));
}

void Options::remove(string name)
{
	_options.erase(
				remove_if(
					_options.begin(),
					_options.end(),
					[name](const OptionNamed& p) { return p.first == name; }),
				_options.end()
				);
}

void Options::clear()
{
	for(OptionNamed opt : _options)
		opt.second->clear();

	_options.clear();
}

void Options::optionsChanged(Option *option)
{
	notifyChanged(option);
}

Json::Value Options::asJSON(bool includeTransient) const
{
	Json::Value top = Json::objectValue;

	for (const OptionNamed & item : _options)
	{
		if (includeTransient == false && item.second->isTransient())
			continue;

		string name			= item.first;
		Json::Value value	= item.second->asJSON();
		insertValue(name, value, top);
	}

	return top;
}


Json::Value Options::asMetaJSON() const
{
	Json::Value top = Json::objectValue;

	for (const OptionNamed & item : _options)
	{
		string name			= item.first;
		Json::Value value	= item.second->asMetaJSON();

		if(!value.isNull() && !(value.isArray() && value.size() == 0) && !(value.isObject() && value.getMemberNames().size() == 0))
			insertValue(name, value, top);
	}

	return top;
}

void Options::set(const Json::Value &json)
{
	for (const OptionNamed & item : _options)
	{
		string		name = item.first;
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
		string itemName	 = name.substr(endPos + 1);

		insertValue(itemName, value, root[groupName]);
	}
	else
		root[name] = value;
}


bool Options::extractValue(const string &name, const Json::Value &root, Json::Value &value)
{
	size_t endPos;

	if ((endPos = name.find('/', 0)) != string::npos)
	{
		string groupName = name.substr(0, endPos);
		string itemName  = name.substr(endPos + 1);

		return extractValue(itemName, root[groupName], value);
	}
	else if (root.isMember(name))
	{
		value = root[name];
		return true;
	}
	else
		return false;
}

Option *Options::get(const string & name) const
{
	for (const OptionNamed& p : _options)
	{
		if (p.first == name)
			return p.second;
	}

	return nullptr;
}


void Options::get(int index, string &name, Option *&option)
{
	const OptionNamed& optionWithName = _options.at(index);

	name	= optionWithName.first;
	option	= optionWithName.second;
}

Option *Options::clone() const
{
	Options *c = new Options();

	for(const OptionNamed &option : _options)
	{
		(void)_options;
		Option *oc = option.second->clone();
		c->add(option.first, oc);
	}

	return c;
}

std::set<std::string> Options::usedVariables() const
{
	std::set<std::string> combined;

	for (const OptionNamed& option : _options)
	{
		std::set<std::string> cols = option.second->usedVariables();
		combined.insert(cols.begin(), cols.end());
	}

	return combined;
}

std::set<std::string> Options::columnsCreated()
{
	std::set<std::string> combined;

	for (const OptionNamed& option : _options)
	{
		std::set<std::string> cols = option.second->columnsCreated();
		combined.insert(cols.begin(), cols.end());
	}

	return combined;
}

void Options::replaceKey(const string &oldKey, const string &newKey)
{
	for (OptionNamed& option : _options)
		if (option.first == oldKey)
			option.first = newKey;
}

void Options::removeUsedVariable(const std::string & var)
{
	for (const OptionNamed& option : _options)
		option.second->removeUsedVariable(var);

	notifyChanged(this);
}

void Options::replaceVariableName(const std::string & oldName, const std::string & newName)
{
	for (const OptionNamed& option : _options)
		option.second->replaceVariableName(oldName, newName);

	notifyChanged(this);
}
