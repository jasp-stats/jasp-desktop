#include "optionstable.h"

#include "boost/foreach.hpp"

using namespace Json;
using namespace std;

OptionsTable::OptionsTable(OptionsRow *rowTemplate)
{
	_template = rowTemplate;
}

Json::Value OptionsTable::asJSON() const
{
	Value v = arrayValue;
	int i = 0;

	BOOST_FOREACH(Options *item, _rows)
		v[i++] = item->asJSON();

	return v;
}

void OptionsTable::set(Json::Value &value)
{
	BOOST_FOREACH(OptionsRow *row, _rows)
		delete row;
	_rows.clear();

	for (int i = 0; i < value.size(); i++)
	{
		OptionsRow *item = static_cast<OptionsRow *>(_template->clone());
		item->set(value[i]);
		_rows.push_back(item);
	}
}

Option *OptionsTable::clone() const
{
	std::cout << "shouldn't be cloning!";
	std::cout.flush();

	return NULL;
}

OptionsRow *OptionsTable::rowTemplate()
{
	return _template;
}

OptionsRow *OptionsTable::at(int index)
{
	return _rows.at(index);
}

size_t OptionsTable::size()
{
	return _rows.size();
}

OptionsRow *OptionsTable::insertAt(std::string name, int index)
{
	OptionsRow* row = dynamic_cast<OptionsRow *>(_template->clone());
	row->setVariable(name);
	insertAt(row, index);
	return row;
}

void OptionsTable::insertAt(OptionsRow *row, int index)
{
	std::vector<OptionsRow *>::iterator itr = _rows.begin();

	for (int i = 0; i < index; i++)
		itr++;

	_rows.insert(itr, row);

	row->changed.connect(boost::bind(&OptionsTable::rowChanged, this));
}

OptionsRow *OptionsTable::removeAt(int index)
{
	std::vector<OptionsRow *>::iterator itr = _rows.begin();

	for (int i = 0; i < index; i++)
		itr++;

	_rows.erase(itr);
}

OptionsRow *OptionsTable::remove(string name)
{
	std::vector<OptionsRow *>::iterator itr = _rows.begin();

	while (itr != _rows.end())
	{
		OptionsRow *row = *itr;

		if (row->variable() == name)
		{
			_rows.erase(itr);
			return *itr;
		}

		itr++;
	}

	return NULL;
}

bool OptionsTable::contains(string name)
{
	BOOST_FOREACH(OptionsRow *row, _rows)
	{
		if (row->variable() == name)
			return true;
	}

	return false;
}

void OptionsTable::rowChanged()
{
	changed(this);
}
