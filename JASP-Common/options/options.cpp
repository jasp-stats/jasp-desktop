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

#include <boost/foreach.hpp>

#include "options/optionlist.h"
#include "options/optionterm.h"
#include "options/optionterms.h"
#include "options/optionnumber.h"
#include "options/optionstable.h"
#include "options/optionstring.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionvariable.h"
#include "options/optionvariables.h"
#include "options/optiondoublearray.h"
#include "options/optionintegerarray.h"
#include "options/optioncomputedcolumn.h"
#include "options/optionvariablesgroups.h"

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

void Options::init(const Json::Value &array)
{
    for (auto itr = array.begin(); itr != array.end(); itr++)
	{
		Json::Value value = (*itr);

		std::string name = value["name"].asString(),
					type = value["type"].asString();

		Option *option = createOption(type);

		if (option != NULL)
		{
			Json::Value &val = value["value"];
			if (val != Json::nullValue)
				option->set(val);	
			else
				option->init(value);
			add(name, option);
		}
		else
		{
            cout << "Unknown data type: '" << type << "' for name '" << name << "'\n";
            cout.flush();
		}
	}
}

Option* Options::createOption(string typeString)
{
	if		(typeString == "List")				return new OptionList();
	else if (typeString == "Term")				return new OptionTerm();
	else if (typeString == "Terms")				return new OptionTerms();
	else if (typeString == "Table")				return new OptionsTable();
	else if (typeString == "Number")			return new OptionNumber();
	else if (typeString == "String")			return new OptionString();
	else if	(typeString == "Boolean")			return new OptionBoolean();
	else if (typeString == "Integer")			return new OptionInteger();
	else if (typeString == "Variable")			return new OptionVariable();
	else if (typeString == "Variables")			return new OptionVariables();
	else if (typeString == "DoubleArray")		return new OptionDoubleArray();
	else if (typeString == "IntegerArray")		return new OptionIntegerArray();
	else if (typeString == "ComputedColumn")	return new OptionComputedColumn();
	else if (typeString == "VariablesGroups")	return new OptionVariablesGroups();

	return NULL;
}

string Options::getType(Option* option) const
{
	if (dynamic_cast<OptionBoolean*>(option))
		return "Boolean";
	if (dynamic_cast<OptionInteger*>(option))
		return "Integer";
	if (dynamic_cast<OptionIntegerArray*>(option))
		return "IntegerArray";
	if (dynamic_cast<OptionList*>(option))
		return "List";
	if (dynamic_cast<OptionNumber*>(option))
		return "Number";
	if (dynamic_cast<OptionsTable*>(option))
		return "Table";
	if (dynamic_cast<OptionString*>(option))
		return "String";
	if (dynamic_cast<OptionVariable*>(option))
		return "Variable";
	if (dynamic_cast<OptionVariables*>(option))
		return "Variables";
	if (dynamic_cast<OptionTerm*>(option))
		return "Term";
	if (dynamic_cast<OptionTerms*>(option))
		return "Terms";
	if (dynamic_cast<OptionVariablesGroups*>(option))
		return "VariablesGroups";

	return "";
}


void Options::add(string name, Option *option)
{
	_options.push_back(OptionNamed(name, option));
	option->changed.connect(							boost::bind( &Options::optionsChanged,							this, _1));
	option->requestComputedColumnCreation.connect(		boost::bind( &Option::notifyRequestComputedColumnCreation,		this, _1));
	option->requestComputedColumnDestruction.connect(	boost::bind( &Option::notifyRequestComputedColumnDestruction,	this, _1));
}

void Options::optionsChanged(Option *)
{
	notifyChanged();
}

Json::Value Options::asJSONWithType(bool includeTransient) const
{
	Json::Value top = Json::arrayValue;

	BOOST_FOREACH(OptionNamed item, _options)
	{
		if (includeTransient == false && item.second->isTransient())
			continue;
		Json::Value jsonValue = Json::objectValue;

		string name = item.first;
		Option* option = item.second;
		jsonValue["name"] = name;
		jsonValue["type"] = getType(option);
		jsonValue["value"] = option->asJSON();
		
		top.append(jsonValue);
	}

	return top;
}


Json::Value Options::asJSON(bool includeTransient) const
{
	Json::Value top = Json::objectValue;

	for(OptionNamed item : _options)
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
	for(OptionNamed item : _options)
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

Option *Options::get(string name) const
{
	for(OptionNamed p : _options)
	{
		if (p.first == name)
			return p.second;
	}

	return NULL;
}


void Options::get(int index, string &name, Option *&option)
{
	OptionNamed optionWithName = _options.at(index);

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

std::set<std::string> Options::usedVariables()
{
	std::set<std::string> combined;

	for(OptionNamed option : _options)
	{
		std::set<std::string> cols = option.second->usedVariables();
		combined.insert(cols.begin(), cols.end());
	}

	return combined;
}

std::set<std::string> Options::columnsCreated()
{
	std::set<std::string> combined;

	for(OptionNamed option : _options)
	{
		std::set<std::string> cols = option.second->columnsCreated();
		combined.insert(cols.begin(), cols.end());
	}

	return combined;
}

void Options::removeUsedVariable(std::string var)
{
	for(OptionNamed option : _options)
		option.second->removeUsedVariable(var);

	notifyChanged();
}

void Options::replaceVariableName(std::string oldName, std::string newName)
{
	for(OptionNamed option : _options)
		option.second->replaceVariableName(oldName, newName);

	notifyChanged();
}
