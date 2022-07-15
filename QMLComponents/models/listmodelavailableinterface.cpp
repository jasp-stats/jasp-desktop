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

#include "listmodelavailableinterface.h"
#include "listmodelassignedinterface.h"
#include "controls/jasplistcontrol.h"
#include "log.h"

void ListModelAvailableInterface::initTerms(const Terms &terms, const RowControlsValues&)
{
	beginResetModel();
	
	_allTerms = _allSortedTerms = terms;
	_setTerms(terms, _allSortedTerms);

	if (currentSortType() != SortType::None)
		Sortable::sortItems();

	removeTermsInAssignedList();
	endResetModel();
}

void ListModelAvailableInterface::sortItems(SortType sortType)
{
	beginResetModel();

	switch(sortType)
	{
	case SortType::None:
	{
		Terms allowed, forbidden;

		for (const Term &term : _allTerms)
		{
			if ( ! isAllowed(term))	forbidden.add(term);
			else					allowed.add(term);
		}

		_allTerms.clear();
		_allTerms.add(allowed);
		_allTerms.add(forbidden);
		_allSortedTerms = _allTerms;
		break;
	}

	case SortType::SortByName:
	{
		QList<QString> sortedTerms = _allSortedTerms.asQList();
		std::sort(sortedTerms.begin(), sortedTerms.end(),
				  [&](const QString& a, const QString& b) {
						return a.compare(b, Qt::CaseInsensitive) < 0;
					});
		_allSortedTerms = Terms(sortedTerms);
		break;
	}

	case SortType::SortByType:
	{
		QList<QString>				termsList = _allSortedTerms.asQList();
		QList<QPair<QString, int> > termsTypeList;

		for (const QString& term : termsList)
			termsTypeList.push_back(QPair<QString, int>(term, requestInfo(VariableInfo::VariableType, term).toInt()));

		std::sort(termsTypeList.begin(), termsTypeList.end(),
				  [&](const QPair<QString, int>& a, const QPair<QString, int>& b) {
						return a.second - b.second > 0;
					});

		QList<QString> sortedTerms;

		for (const auto& term : termsTypeList)
			sortedTerms.push_back(term.first);

		_allSortedTerms = Terms(sortedTerms);
		break;
	}

	default:
		Log::log() << "Unimplemented sort in ListModelAvailableInterface::sortItems!";
		break;
	}

	Terms orgTerms = terms();
	_setTerms(orgTerms); // This will reorder the terms

	endResetModel();
}

void ListModelAvailableInterface::sourceTermsReset()
{
	resetTermsFromSources();
}

void ListModelAvailableInterface::sourceNamesChanged(QMap<QString, QString> map)
{
	ListModelDraggable::sourceNamesChanged(map);

	QMap<QString, QString>	allTermsChangedMap;
	QMapIterator<QString, QString> it(map);

	while (it.hasNext())
	{
		it.next();
		const QString& oldName = it.key(), newName = it.value();

		QSet<int> allIndexes = _allTerms.replaceVariableName(oldName.toStdString(), newName.toStdString());

		if (allIndexes.size() > 0)
			allTermsChangedMap[oldName] = newName;
	}

	if (allTermsChangedMap.size() > 0)
		emit namesChanged(allTermsChangedMap);
}

void ListModelAvailableInterface::sourceColumnsChanged(QStringList columns)
{
	ListModelDraggable::sourceColumnsChanged(columns);

	QStringList changedColumns;

	for (const QString& column : columns)
	{
		if (_allTerms.contains(column))
			changedColumns.push_back(column);
	}

	if (changedColumns.size() > 0)
		emit columnsChanged(changedColumns);
}

int ListModelAvailableInterface::sourceColumnTypeChanged(QString name)
{
	int index = ListModelDraggable::sourceColumnTypeChanged(name);

	if (index == -1 && _allTerms.contains(name))
		emit columnTypeChanged(name);

	return index;
}

bool ListModelAvailableInterface::sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels)
{
	bool change = ListModelDraggable::sourceLabelsChanged(columnName, changedLabels);

	if (!change && _allTerms.contains(columnName))
		emit labelsChanged(columnName, changedLabels);

	return change;
}

bool ListModelAvailableInterface::sourceLabelsReordered(QString columnName)
{
	bool change = ListModelDraggable::sourceLabelsReordered(columnName);

	if (!change && _allTerms.contains(columnName))
		emit labelsReordered(columnName);

	return change;
}

void ListModelAvailableInterface::removeTermsInAssignedList()
{
	beginResetModel();
	
	Terms newTerms = _allSortedTerms;
	
	for (ListModelAssignedInterface* modelAssign : assignedModel())
	{
		Terms assignedTerms = modelAssign->terms();
		if (assignedTerms.discardWhatIsntTheseTerms(_allSortedTerms))
			modelAssign->initTerms(assignedTerms); // initTerms call removeTermsInAssignedList
		else if (!modelAssign->copyTermsWhenDropped())
			newTerms.remove(assignedTerms);
	}

	_setTerms(newTerms, _allSortedTerms);
	
	endResetModel();
}

void ListModelAvailableInterface::addAssignedModel(ListModelAssignedInterface *assignedModel)
{
	_assignedModels.push_back(assignedModel);

	connect(assignedModel,	&ListModelAssignedInterface::destroyed,				this,						&ListModelAvailableInterface::removeAssignedModel		);
	connect(this,			&ListModelAvailableInterface::availableTermsReset,	assignedModel,				&ListModelAssignedInterface::availableTermsResetHandler	);
	connect(this,			&ListModelAvailableInterface::namesChanged,			assignedModel,				&ListModelAssignedInterface::sourceNamesChanged			);
	connect(this,			&ListModelAvailableInterface::columnsChanged,		assignedModel,				&ListModelAssignedInterface::sourceColumnsChanged		);
	connect(this,			&ListModelAvailableInterface::columnTypeChanged,	assignedModel,				&ListModelAssignedInterface::sourceColumnTypeChanged	);
	connect(this,			&ListModelAvailableInterface::labelsChanged,		assignedModel,				&ListModelAssignedInterface::sourceLabelsChanged		);
	connect(this,			&ListModelAvailableInterface::labelsReordered,		assignedModel,				&ListModelAssignedInterface::sourceLabelsReordered		);
	connect(listView(),		&JASPListControl::containsVariablesChanged,			assignedModel->listView(),	&JASPListControl::setContainsVariables					);
	connect(listView(),		&JASPListControl::containsInteractionsChanged,		assignedModel->listView(),	&JASPListControl::setContainsInteractions				);
}

void ListModelAvailableInterface::removeAssignedModel(ListModelDraggable* assignedModel)
{
	_assignedModels.removeAll(qobject_cast<ListModelAssignedInterface*>(assignedModel));
}

