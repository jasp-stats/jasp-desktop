//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "boundcontrollayers.h"
#include "listmodellayersassigned.h"
#include "jasplistcontrol.h"
#include "analysis/analysisform.h"
#include "utilities/qutils.h"

using namespace std;


BoundControlLayers::BoundControlLayers(ListModelLayersAssigned* model) : BoundControlBase(model->listView())
{
	_layersModel = model;
}

void BoundControlLayers::bindTo(const Json::Value &value)
{
	BoundControlBase::bindTo(value);
	vector<vector<string> > variables;	
	
	for (const Json::Value& row : value)
	{
		const Json::Value& rowVariables = row["variables"];
		vector<string> rowValues;

		if (rowVariables.isString())
			rowValues.push_back(rowVariables.asString());
		else if (rowVariables.isArray())
		{
			for (const Json::Value& rowVariable : rowVariables)
				rowValues.push_back(rowVariable.asString());
		}

		variables.push_back(rowValues);
	}
	
	_layersModel->initLayers(variables);
}

Json::Value BoundControlLayers::createJson()
{	
	return Json::Value(Json::arrayValue);
}

bool BoundControlLayers::isJsonValid(const Json::Value &optionValue)
{
	bool valid = optionValue.type() == Json::arrayValue;
	if (valid)
	{
		for (uint i = 0; i < optionValue.size(); i++)
		{
			const Json::Value& value = optionValue[i];
			valid = value.type() == Json::objectValue;
			if (valid)
			{
				const Json::Value& nameOption = value["name"];
				const Json::Value& variablesOption = value["variables"];
				valid = nameOption.type() == Json::stringValue && variablesOption.type() == Json::arrayValue;

				if (!valid)
					break;
			}
		}
	}

	return valid;
}

void BoundControlLayers::updateOption()
{
	vector<pair<string, vector<string> > > layers = _layersModel->getLayers();
	Json::Value boundValue(Json::arrayValue);
	
	for (const auto &layer : layers)
	{
		Json::Value rowValues(Json::objectValue);
		rowValues["name"] = layer.first;

		Json::Value rowVariables(Json::arrayValue);
		for (const string &variable : layer.second)
			rowVariables.append(variable);
		rowValues["variables"] = rowVariables;
		boundValue.append(rowValues);
	}
	
	setBoundValue(boundValue);
}
