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

#include "listmodelassignedinterface.h"
#include "controls/variableslistbase.h"
#include "analysisform.h"

ListModelAssignedInterface::ListModelAssignedInterface(JASPListControl* listView)
	: ListModelDraggable(listView)
  , _availableModel(nullptr)
{
	_needsSource = false;
}

void ListModelAssignedInterface::refresh()
{
	bool doRefresh = true;
	QList<int> toRemove;
	for (int i = 0; i < rowCount(); i++)
	{
		QString term = data(index(i, 0)).toString();
		if (!isAllowed(term))
			toRemove.push_back(i);
	}

	if (toRemove.count() > 0)
	{
		VariablesListBase* qmlListView = dynamic_cast<VariablesListBase*>(listView());
		if (qmlListView)
		{
			qmlListView->moveItems(toRemove, _availableModel);
			doRefresh = false;
		}
	}

	if (doRefresh)
		ListModelDraggable::refresh();
}

void ListModelAssignedInterface::setAvailableModel(ListModelAvailableInterface *source)
{
	_availableModel = source;
}

int ListModelAssignedInterface::sourceColumnTypeChanged(QString name)
{
	int index = ListModelDraggable::sourceColumnTypeChanged(name);
	VariablesListBase* qmlListView = dynamic_cast<VariablesListBase*>(listView());

	if (qmlListView && index >= 0 && index < int(terms().size()))
	{
		if (!isAllowed(terms().at(size_t(index))))
		{
			QList<int> indexes = {index};
			qmlListView->moveItems(indexes, _availableModel);
			ListModelDraggable::refresh();
		}
		// Force the analysis to be rerun
		emit qmlListView->boundValueChanged(qmlListView);
	}

	return index;
}

bool ListModelAssignedInterface::sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels)
{
	bool change = ListModelDraggable::sourceLabelsChanged(columnName, changedLabels);

	if (change && listView() && listView()->form())
		listView()->form()->refreshAnalysis();

	return change;
}

bool ListModelAssignedInterface::sourceLabelsReordered(QString columnName)
{
	bool change = ListModelDraggable::sourceLabelsReordered(columnName);

	if (change && listView() && listView()->form())
		listView()->form()->refreshAnalysis();

	return change;
}

void ListModelAssignedInterface::sourceTermsReset()
{
	ListModelDraggable::sourceTermsReset();
	if (!_rowComponent) return;

	// If row components exist and source changes, the default value must change.
	// Each new term should give the default values for their row
	// If the term already exists, then the default value for this row should not change.
	BoundControl* boundControl = listView()->boundControl();
	if (!boundControl) return;

	const Json::Value& defaultValue = boundControl->defaultBoundValue();
	const Json::Value& newValue = boundControl->boundValue();

	if (!newValue.isArray() || !defaultValue.isArray()) return; // should never happen..

	Json::Value newDefaultValue(Json::arrayValue);
	std::string optionKey = fq(listView()->optionKey());
	for (const Json::Value& rowValue : newValue)
	{
		if (!rowValue.isObject()) continue; //should never happen
		const Json::Value& key = rowValue[optionKey];
		bool foundDefaultValue = false;
		for (const Json::Value& defaultRowValue : defaultValue)
		{
			if (!defaultRowValue.isArray()) continue; //should never happen
			if (defaultRowValue[optionKey] == key)
			{
				foundDefaultValue = true;
				newDefaultValue.append(defaultRowValue);
				break;
			}
		}
		if (!foundDefaultValue)
			newDefaultValue.append(rowValue);
	}

	boundControl->setDefaultBoundValue(newDefaultValue);
}
