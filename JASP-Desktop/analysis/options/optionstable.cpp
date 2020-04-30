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

Json::Value OptionsTable::asMetaJSON() const
{
	Json::Value v(Json::arrayValue);

	for(Options *item : _value)
		v.append(item->asMetaJSON());

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
			row->changed.connect(boost::bind( &OptionsTable::optionsChanged,	this, _1));
			_value.push_back(row);
		}
	}
	if (!_template)
		_cachedValue = value; // keep the value until a template is given
}

Option *OptionsTable::clone() const
{
	OptionsTable * c = new OptionsTable(_template == nullptr ? nullptr : static_cast<Options*>(_template->clone()), _cachedValue);

	std::vector<Options *> rows;

	for(Options *row : _value)
		rows.push_back(static_cast<Options*>(row->clone()));

	c->setValue(rows);

	return c;
}

void OptionsTable::setValue(const std::vector<Options *> &value)
{
	std::set<Options *> newOnes(value.begin(),	value.end());

	//Ok we do some checks to make sure that we arent setting the value to the same value. Because in that case some forms/analyses might go crazy restarting constantly (because of the way ListModelFilteredDataEntry sets the options... Very bluntly
	bool changesFound = value.size() != _value.size();

	/*for(Options * & older : _value)
		if(newOnes.count(older) == 0)
		{
			delete older;
			older = nullptr;
		}

	if(!changesFound)
		changesFound = value.size() != _value.size(); //size might have changed
*/
	if(!changesFound)
		for(size_t i=0; i<_value.size() && !changesFound; i++)
			if(
				((_value[i] == nullptr || value[i] == nullptr) && _value[i] != value[i])
				||
				(_value[i]->asJSON().toStyledString() != value[i]->asJSON().toStyledString())
			)
				changesFound = true;

	_value = value;

	if(changesFound)
		notifyChanged(this);
}

void OptionsTable::connectOptions(const std::vector<Options *> &value)
{
	setValue(value);
	
	for (Options* options : _value)
		options->changed.connect(boost::bind( &OptionsTable::optionsChanged,	this, _1));	
}

std::set<std::string> OptionsTable::usedVariables() const
{
	std::set<std::string> combined;

	for (const Options * options : _value)
	{
		std::set<std::string> cols = options->usedVariables();
		combined.insert(cols.begin(), cols.end());
	}

	return combined;
}

void OptionsTable::replaceKey(const std::string& oldKey, const std::string& newKey)
{
	if (_template)
		_template->replaceKey(oldKey, newKey);

	for (Options* options : _value)
		options->replaceKey(oldKey, newKey);
}
