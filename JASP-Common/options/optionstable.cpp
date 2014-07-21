#include "optionstable.h"

#include "boost/foreach.hpp"

#include "optionvariable.h"

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

	BOOST_FOREACH(Options *item, _value)
		v[i++] = item->asJSON();

	return v;
}

void OptionsTable::set(Json::Value &value)
{
	BOOST_FOREACH(Options *row, _value)
		delete row;
	_value.clear();

	for (uint i = 0; i < value.size(); i++)
	{
		Options *row = static_cast<Options *>(_template->clone());
		row->set(value[i]);
		_value.push_back(row);

		//row->changed.connect(boost::bind(&OptionsTable::rowChanged, this));
	}
}

Option *OptionsTable::clone() const
{
	std::cout << "shouldn't be cloning!";
	std::cout.flush();

	return NULL;
}

void OptionsTable::setValue(vector<Options *> value)
{
	_value = value;
	notifyChanged();
}

Options *OptionsTable::rowTemplate() const
{
	return _template;
}

/*Options *OptionsTable::at(int index) const
{
	return _value.at(index);
}

size_t OptionsTable::size() const
{
	return _value.size();
}

void OptionsTable::rowChanged()
{
	notifyChanged();
}*/
