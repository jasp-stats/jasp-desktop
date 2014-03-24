#include "optionstable.h"

#include "boost/foreach.hpp"

#include "optionfield.h"

using namespace Json;
using namespace std;

OptionsTable::OptionsTable(Options *rowTemplate)
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
	BOOST_FOREACH(Options *row, _rows)
		delete row;
	_rows.clear();

	for (int i = 0; i < value.size(); i++)
	{
		Options *item = static_cast<Options *>(_template->clone());
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

Options *OptionsTable::rowTemplate()
{
	return _template;
}

Options *OptionsTable::at(int index)
{
	return _rows.at(index);
}

size_t OptionsTable::size()
{
	return _rows.size();
}

void OptionsTable::insert(int index, Options *row)
{
	std::vector<Options *>::iterator itr = _rows.begin();

	for (int i = 0; i < index; i++)
		itr++;

	_rows.insert(itr, row);

	row->changed.connect(boost::bind(&OptionsTable::rowChanged, this));
}

Options *OptionsTable::remove(int index)
{
	std::vector<Options *>::iterator itr = _rows.begin();

	for (int i = 0; i < index; i++)
		itr++;

	_rows.erase(itr);

	rowChanged();

	return *itr;
}

Options *OptionsTable::remove(string name)
{
	std::vector<Options *>::iterator itr = _rows.begin();

	while (itr != _rows.end())
	{
		Options *row = *itr;
		OptionField *option = static_cast<OptionField *>(row->get(0));
		string variable = option->value()[0];

		if (variable == name)
		{
			_rows.erase(itr);
			return *itr;
			rowChanged();
		}

		itr++;
	}

	return NULL;
}

bool OptionsTable::contains(string name)
{
	BOOST_FOREACH(Options *row, _rows)
	{
		OptionField *option = static_cast<OptionField *>(row->get(0));
		string variable = option->value()[0];

		if (variable == name)
			return true;
	}

	return false;
}

void OptionsTable::rowChanged()
{
	changed(this);
}
