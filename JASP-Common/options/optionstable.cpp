#include "optionstable.h"

#include "boost/foreach.hpp"

#include "optionvariable.h"

using namespace Json;
using namespace std;

OptionsTable::OptionsTable(Options *rowTemplate)
{
	_template = rowTemplate;
}

OptionsTable::OptionsTable()
{
	_template = NULL;
}

void OptionsTable::init(const Json::Value &data)
{
	_template = new Options();

	Json::Value templ4te = data.get("template", Json::nullValue);

	if (templ4te.isNull() == false)
		_template->init(templ4te);

	Json::Value d3fault = data.get("default", Json::nullValue);

	if (d3fault.isNull() == false)
		set(d3fault);
}

Json::Value OptionsTable::asJSON() const
{
	Value v = arrayValue;
	int i = 0;

	BOOST_FOREACH(Options *item, _value)
		v[i++] = item->asJSON();

	return v;
}

void OptionsTable::set(const Json::Value &value)
{
	BOOST_FOREACH(Options *row, _value)
		delete row;
	_value.clear();

	for (uint i = 0; i < value.size(); i++)
	{
		Options *row = static_cast<Options *>(_template->clone());
		row->set(value[i]);
		_value.push_back(row);
	}
}

Option *OptionsTable::clone() const
{
	std::cout << "shouldn't be cloning!";
	std::cout.flush();

	return NULL;
}

void OptionsTable::setValue(const vector<Options *> &value)
{
	_value = value;
	notifyChanged();
}

Options *OptionsTable::rowTemplate() const
{
	return _template;
}
