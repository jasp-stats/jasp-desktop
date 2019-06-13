//
// Copyright (C) 2013-2019 University of Amsterdam
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
#include "optionvariable.h"


void OptionsTable::init(const Json::Value &data)
{
				_template		= new Options();
	Json::Value templateJson	= data.get("template", Json::nullValue);
	Json::Value defaultJson		= data.get("default", Json::nullValue);

	if (!templateJson.isNull())
		_template->init(templateJson);


	if (!defaultJson.isNull())
		set(defaultJson);
}

Json::Value OptionsTable::asJSON() const
{
	Json::Value v(Json::arrayValue);

	for(Options *item : _value)
		v.append(item->asJSON());

	return v;
}

void OptionsTable::deleteOldValues()
{
	for(Options *row : _value)
		delete row;

	_value.clear();
}

void OptionsTable::set(const Json::Value &value)
{
	deleteOldValues();

	if (_template)
	{
		for (uint i = 0; i < value.size(); i++)
		{
			Options *row = static_cast<Options *>(_template->clone());
			row->set(value[i]);
			_value.push_back(row);
		}
	}
	else
		_cachedValue = value; // keep the value until a template is given
}

Option *OptionsTable::clone() const
{
	OptionsTable * c = new OptionsTable(_template == nullptr ? nullptr : static_cast<Options*>(_template->clone()));

	std::vector<Options *> rows;

	for(Options *row : _value)
		rows.push_back(static_cast<Options*>(row->clone()));

	c->setValue(rows);

	return c;
}

void OptionsTable::setValue(const std::vector<Options *> &value)
{
	deleteOldValues();

	_value = value;
	notifyChanged();
}

void OptionsTable::connectOptions(const std::vector<Options *> &value)
{
	setValue(value);
	
	for (Options* options : value)
		options->changed.connect(boost::bind( &OptionsTable::optionsChanged,	this, _1));	
}



void OptionsTable::setTemplate(Options *templote)
{
	_template = templote;
	
	if(!_cachedValue.isNull())
	{
		set(_cachedValue);
		_cachedValue = Json::nullValue;
	}
}
