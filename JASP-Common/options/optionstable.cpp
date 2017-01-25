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

#include "optionstable.h"

#include "boost/foreach.hpp"

#include "optionvariable.h"

using namespace Json;
using namespace std;

OptionsTable::OptionsTable(Options *rowTemplate)
	: OptionI(true)
{
	_template = rowTemplate;
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
	Options *rowTemplate = static_cast<Options*>(this->rowTemplate()->clone());

	OptionsTable *c = new OptionsTable(rowTemplate);

	std::vector<Options *> rows;

	BOOST_FOREACH(Options *row, _value)
		rows.push_back(static_cast<Options*>(row->clone()));

	c->setValue(rows);

	return c;
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
