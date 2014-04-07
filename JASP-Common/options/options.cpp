#include "options.h"

#include <QStringBuilder>
#include <QDebug>

#include <boost/foreach.hpp>


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

void Options::add(string name, Option *option)
{
	_options.push_back(OptionNamed(name, option));
	option->changed.connect(boost::bind(&Options::optionsChanged, this));
}

void Options::optionsChanged()
{
	changed(this);
}

Json::Value Options::asJSON() const
{
	Json::Value top = Json::objectValue;

	BOOST_FOREACH(OptionNamed item, _options)
	{
		string name = item.first;
		Json::Value value = item.second->asJSON();
		insertValue(name, value, top);
	}

	return top;
}

void Options::set(Json::Value &json)
{
	BOOST_FOREACH(OptionNamed item, _options)
	{
		string name = item.first;
		Json::Value value;
		if (extractValue(name, json, value))
			item.second->set(value);
	}

	optionsChanged();
}

void Options::insertValue(string &name, Json::Value &value, Json::Value &root)
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

bool Options::extractValue(string &name, Json::Value &root, Json::Value &value)
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

size_t Options::size()
{
	return _options.size();
}

