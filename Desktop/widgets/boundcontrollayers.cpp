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
#include "analysis/analysisform.h"
#include "utilities/qutils.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"

#include <QQmlProperty>

using namespace std;


BoundControlLayers::BoundControlLayers(ListModelLayersAssigned* model)
{
	_boundTo = nullptr;
	_layersModel = model;
}

void BoundControlLayers::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);
	
	vector<Options*> allOptions = _boundTo->value();
	vector<vector<string> > variables;	
	
	for (const Options* options : allOptions)
	{
		OptionVariables *variablesOption = static_cast<OptionVariables *>(options->get("variables"));
		variables.push_back(variablesOption->variables());
	}
	
	_layersModel->initLayers(variables);
}

Option* BoundControlLayers::createOption()
{
	Options* templote = new Options();
	templote->add("name", new OptionString());
	templote->add("variables", new OptionVariables());
	
	OptionsTable* optionsTable = new OptionsTable(templote);
	
	return optionsTable;
}

bool BoundControlLayers::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
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
	vector<Options *> allOptions;
	
	for (const auto &layer : layers)
	{
		Options* options = new Options();
		options->add("name", new OptionString(layer.first));
		OptionVariables* optionVariables = new OptionVariables();
		vector<string> variables;
		for (const string &variable : layer.second)
			variables.push_back(variable);
		optionVariables->setValue(variables);
		options->add("variables", optionVariables);
		allOptions.push_back(options);
	}
	
	_boundTo->setValue(allOptions);	
}
