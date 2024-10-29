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
#include "controls/sourceitem.h"

#include "log.h"

void ListModelAvailableInterface::initTerms(const Terms &terms, const RowControlsValues&, bool)
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
		std::sort(_allSortedTerms.begin(), _allSortedTerms.end(),
				  [&](const Term& a, const Term& b) {
						return a.asQString().compare(b.asQString(), Qt::CaseInsensitive) < 0;
					});
		break;
	}

	case SortType::SortByType:
	{
		std::sort(_allSortedTerms.begin(), _allSortedTerms.end(),
				  [&](const Term& a, const Term& b) {
						return int(a.type()) - int(b.type()) > 0;
					});
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

Terms ListModelAvailableInterface::addTerms(const Terms &terms, int dropItemIndex, const RowControlsValues &rowValues)
{
	if (listView()->sourceItems().length() > 0 && listView()->sourceItems()[0]->isAnalysisDataSet())
	{
		// Reset the real types to the terms, in case they were changed.
		Terms realTypesTerms = terms;
		for (Term& term : realTypesTerms)
			term.setType(getVariableRealType(term.asQString()));
		return ListModelDraggable::addTerms(realTypesTerms, dropItemIndex, rowValues);

	}

	return ListModelDraggable::addTerms(terms, dropItemIndex, rowValues);
}

void ListModelAvailableInterface::sourceTermsReset()
{
	resetTermsFromSources();
}

void ListModelAvailableInterface::sourceNamesChanged(QMap<QString, QString> map)
{
	ListModelDraggable::sourceNamesChanged(map);

	// Not only the terms must be changed, but also the allTerms: allTerms keeps all terms that an
	// available model can have: this is its own terms and the terms assigned in its assigned models.
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

bool ListModelAvailableInterface::sourceColumnTypeChanged(Term term)
{
	bool change = ListModelDraggable::sourceColumnTypeChanged(term);

	if (!change && _allTerms.contains(term))
		emit columnTypeChanged(term);

	return change;
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
	if (keepTerms())
		return;

	Terms	oldTerms = terms(),
			newTerms = _allSortedTerms;

	for (ListModelAssignedInterface* modelAssign : assignedModels())
	{
		Terms assignedTerms = modelAssign->terms();
		if (assignedTerms.discardWhatIsntTheseTerms(_allSortedTerms))
			modelAssign->initTerms(assignedTerms, RowControlsValues(), true); // initTerms call removeTermsInAssignedList
		newTerms.remove(assignedTerms);
	}

	if (oldTerms == newTerms)
		return;

	beginResetModel();

	_setTerms(newTerms, _allSortedTerms);
	
	endResetModel();
}

void ListModelAvailableInterface::addAssignedModel(ListModelAssignedInterface *assignedModel)
{
	_assignedModels.push_back(assignedModel);

	connect(this,			&ListModelAvailableInterface::availableTermsReset,	assignedModel,				&ListModelAssignedInterface::availableTermsResetHandler	);
	connect(this,			&ListModelAvailableInterface::namesChanged,			assignedModel,				&ListModelAssignedInterface::sourceNamesChanged			);
	connect(this,			&ListModelAvailableInterface::columnsChanged,		assignedModel,				&ListModelAssignedInterface::sourceColumnsChanged		);
	connect(this,			&ListModelAvailableInterface::columnTypeChanged,	assignedModel,				&ListModelAssignedInterface::sourceColumnTypeChanged	);
	connect(this,			&ListModelAvailableInterface::labelsChanged,		assignedModel,				&ListModelAssignedInterface::sourceLabelsChanged		);
	connect(this,			&ListModelAvailableInterface::labelsReordered,		assignedModel,				&ListModelAssignedInterface::sourceLabelsReordered		);
	connect(this,			&ListModelAvailableInterface::filterChanged,		assignedModel,				&ListModelAssignedInterface::filterChanged				);
	connect(listView(),		&JASPListControl::containsVariablesChanged,			assignedModel->listView(),	&JASPListControl::setContainsVariables					);
	connect(listView(),		&JASPListControl::containsInteractionsChanged,		assignedModel->listView(),	&JASPListControl::setContainsInteractions				);
}

void ListModelAvailableInterface::removeAssignedModel(ListModelAssignedInterface *assignedModel)
{
	_assignedModels.removeAll(assignedModel);
}

