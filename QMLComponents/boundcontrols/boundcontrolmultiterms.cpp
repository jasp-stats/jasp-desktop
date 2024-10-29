//
// Copyright (C) 2013-2020 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "boundcontrolmultiterms.h"
#include "controls/jasplistcontrol.h"
#include "models/listmodelmultitermsassigned.h"

BoundControlMultiTerms::BoundControlMultiTerms(ListModelMultiTermsAssigned* listModel) : BoundControlBase(listModel->listView())
{
	_listModel = listModel;
}

void BoundControlMultiTerms::bindTo(const Json::Value& value)
{
	Json::Value adjustedValue = _isValueWithTypes(value) ? value["value"] : value;

	BoundControlBase::bindTo(adjustedValue);

	std::vector<std::vector<std::string> > values;

	for (const Json::Value& rowJson : adjustedValue)
	{
		std::vector<std::string> rowValues;
		if (rowJson.isArray())
		{
			for (const Json::Value& val : rowJson)
				rowValues.push_back(val.asString());
		} else if (rowJson.isString())
			rowValues.push_back(rowJson.asString());

		values.push_back(rowValues);
	}

	_listModel->initTerms(values);
}

Json::Value BoundControlMultiTerms::createJson() const
{
	return Json::Value(Json::arrayValue);
}

bool BoundControlMultiTerms::isJsonValid(const Json::Value &optionValue) const
{
	return optionValue.type() == Json::arrayValue || optionValue.type() == Json::objectValue;
}

void BoundControlMultiTerms::resetBoundValue()
{
	const QList<Terms>& tuples = _listModel->tuples();
	Json::Value boundValue(Json::arrayValue);
	for (const Terms& terms : tuples)
	{
		Json::Value rowValue;
		for (std::string val : terms.asVector())
			rowValue.append(val);
		boundValue.append(rowValue);
	}

	setBoundValue(boundValue);
}

void BoundControlMultiTerms::setBoundValue(const Json::Value &value, bool emitChanges)
{
	Json::Value newValue;

	if (_control->encodeValue())
	{
		if (_isValueWithTypes(value))
			newValue = value;
		else
		{
			// Else we are loading from a jasp version before "preloadData" was on or var.types were added to the options
			Json::Value types(Json::arrayValue);
			std::string type =  columnTypeToString(_listModel->listView()->defaultType());
			for (const Json::Value& row : value)
			{
				Json::Value rowType(Json::arrayValue);
				if (row.isString())
					rowType.append(type);
				else if (row.isArray())
				{
					for (int i = 0; i < row.size(); i++)
						rowType.append(type);
				}
				types.append(rowType);
			}
			newValue["value"] = value;
			newValue["types"] = types;
		}
	}

	BoundControlBase::setBoundValue(newValue.isNull() ? value : newValue, emitChanges);

}
