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

#include "boundqmlnetworkfactors.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"

#include <QQmlProperty>
#include <QQuickItem>

BoundQMLNetworkFactors::BoundQMLNetworkFactors(QQuickItem *item, AnalysisForm *form)
	: QMLItem(item, form)
	, QMLListView(item, form)
	, BoundQMLItem()
{
	_groupsModel = new ListModelNetworkFactors(this);
	setTermsAreNotVariables();
	
	QQuickItem::connect(_item,	SIGNAL(itemChanged(int, QVariant)),	_groupsModel,	SLOT(itemChanged(int, QVariant)));
	QQuickItem::connect(_item,	SIGNAL(itemRemoved(int)),			_groupsModel,	SLOT(itemRemoved(int)));
}

void BoundQMLNetworkFactors::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable*>(option);
	std::vector<Options*> allOptions = _boundTo->value();

	std::vector<std::string> groups;
	for (const Options* options : allOptions)
	{
		OptionVariables *groupNameOption = static_cast<OptionVariables *>(options->get("group"));
		groups = groupNameOption->variables();
	}
	_groupsModel->initGroups(groups);

}

Option* BoundQMLNetworkFactors::createOption()
{
	Options* templote = new Options();
	templote->add("group", new OptionVariables());
	
	OptionsTable* optionsTable = new OptionsTable(templote);
	
	Options* options = new Options();
	std::vector<std::string> firstGroups = {"Group 1", "Group 2"};
	OptionVariables* groups = new OptionVariables();
	
	groups->setValue(firstGroups);
	options->add("group", groups);
	
	std::vector<Options*> allOptions;
	allOptions.push_back(options);
	optionsTable->connectOptions(allOptions);
	
	return optionsTable;
}

bool BoundQMLNetworkFactors::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
}

bool BoundQMLNetworkFactors::isJsonValid(const Json::Value &optionValue)
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
				const Json::Value& nameOption = value["group"];
				valid = nameOption.type() == Json::stringValue;

				if (!valid)
					break;
			}
		}
	}

	return valid;
}

void BoundQMLNetworkFactors::modelChangedHandler()
{
	const std::vector<std::string> &groups = _groupsModel->getGroups();
	std::vector<Options *> allOptions;
	
	for (const std::string &group : groups)
	{
		Options* options = new Options();
		options->add("group", new OptionString(group));
		allOptions.push_back(options);
	}
	_boundTo->setValue(allOptions);	
	
}
