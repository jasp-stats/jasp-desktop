#include "options.h"

#include <QStringBuilder>
#include <QDebug>

#include <boost/foreach.hpp>


using namespace std;

typedef std::pair<std::string, Option*> OptionWithName;

Options::Options()
{

}

void Options::add(Option *option)
{
	_store.insert(OptionWithName(option->name(), option));
	option->changed.connect(boost::bind(&Options::optionsChanged, this));
}

void Options::optionsChanged()
{
	onChange();
}

Json::Value Options::asJSON() const
{
	Json::Value top = Json::Value(Json::objectValue);

	BOOST_FOREACH(OptionWithName item, _store)
	{
		string name = item.first;
		Json::Value value = item.second->asJSON();
		insertValue(name, value, top);
	}

	return top;
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

Option *Options::get(string name) const
{
	return _store.at(name);
}

Options::iterator Options::begin()
{
	if (_options.size() == 0)
	{
		BOOST_FOREACH(OptionWithName item, _store)
		{
			_options.push_back(item.second);
		}
	}

	return _options.begin();
}

Options::iterator Options::end()
{
	if (_options.size() == 0)
		begin();

	return _options.end();
}
