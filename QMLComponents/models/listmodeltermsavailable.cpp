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

#include "listmodeltermsavailable.h"
#include "listmodelassignedinterface.h"
#include "controls/jasplistcontrol.h"
#include "controls/sourceitem.h"
#include "analysisform.h"
#include "filter.h"
#include "log.h"

ListModelTermsAvailable::ListModelTermsAvailable(JASPListControl *listView, const Terms &terms)
	: ListModelDraggable(listView)
{
	_setTerms(terms);
}

void ListModelTermsAvailable::initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm&)
{
	beginResetModel();
	
	_allTerms = _allSortedTerms = terms;
	_setTerms(terms, _allSortedTerms);

	if (currentSortType() != SortType::None)
		Sortable::sortItems();

	endResetModel();
	
	removeTermsInAssignedList();
}

void ListModelTermsAvailable::sortItems(SortType sortType)
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
						return a.label().compare(b.label(), Qt::CaseInsensitive) < 0;
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
		Log::log() << "Unimplemented sort in ListModelTermsAvailable::sortItems!";
		break;
	}

	Terms orgTerms = terms();
	_setTerms(orgTerms); // This will reorder the terms

	endResetModel();
}

Terms ListModelTermsAvailable::addTerms(const Terms &terms, int dropItemIndex, const Terms::RelatedValuesPerTerm &rowValues)
{
	if (listView()->sourceItems().length() > 0 && listView()->sourceItems()[0]->isAnalysisDataSet())
	{
		// Reset the real types to the terms, in case they were changed.
		Terms realTypesTerms = terms;
		for (Term& term : realTypesTerms)
			term.setType(getVariableRealType(term.value()));
		return ListModelDraggable::addTerms(realTypesTerms, dropItemIndex, rowValues);

	}

	return ListModelDraggable::addTerms(terms, dropItemIndex, rowValues);
}

void ListModelTermsAvailable::sourceTermsReset()
{
	Terms	oldTerms = terms(),
			newTerms,
			removedTerms, addedTerms;

	if (listView()->addEmptyValue())
		newTerms.add(Term("", listView()->placeholderText(), ""));

	newTerms.add(getSourceTerms());

	for (const Term& term : _allTerms)
		if (!newTerms.containsValue(term))
			removedTerms.add(term);

	for (const Term& term : newTerms)
		if (!_allTerms.containsValue(term))
			addedTerms.add(term);

	if (oldTerms != newTerms)
	{
		initTerms(newTerms);
		_connectAllSourcesControls();
	}


	if (addedTerms.size() > 0 || removedTerms.size() > 0)
		emit availableTermsReset(addedTerms, removedTerms);
}

void ListModelTermsAvailable::sourceVariableNamesChanged(QMap<QString, QString> map)
{
	ListModelDraggable::sourceVariableNamesChanged(map);

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
		emit variableNamesChanged(allTermsChangedMap);
}

void ListModelTermsAvailable::sourceVariablesChanged(QStringList columns)
{
	ListModelDraggable::sourceVariablesChanged(columns);

	QStringList changedColumns;

	for (const QString& column : columns)
	{
		if (_allTerms.containsValue(column))
			changedColumns.push_back(column);
	}

	if (changedColumns.size() > 0)
		emit variablesChanged(changedColumns);
}

bool ListModelTermsAvailable::sourceVariableTypeChanged(QString columnName, columnType colType)
{
	Term term(columnName, colType);

	bool change = ListModelDraggable::sourceVariableTypeChanged(columnName, colType);

	if (!change && _allTerms.containsValue(term))
		emit variableTypeChanged(columnName, colType);

	return change;
}

bool ListModelTermsAvailable::sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels)
{
	bool change = ListModelDraggable::sourceLabelsChanged(columnName, changedLabels);

	if (!change && _allTerms.containsValue(columnName))
		emit labelsChanged(columnName, changedLabels);

	return change;
}

bool ListModelTermsAvailable::sourceLabelsReordered(QString columnName)
{
	bool change = ListModelDraggable::sourceLabelsReordered(columnName);

	if (!change && _allTerms.containsValue(columnName))
		emit labelsReordered(columnName);

	return change;
}


void ListModelTermsAvailable::removeTermsInAssignedList()
{
	if (keepTerms())
		return;

	Terms	oldTerms = terms(),
			newTerms = _allSortedTerms;

	for (ListModelAssignedInterface* modelAssign : assignedModels())
	{
		Terms assignedTerms = modelAssign->terms();
		if (assignedTerms.discardWhatIsntTheseTerms(_allSortedTerms))
            modelAssign->initTerms(assignedTerms); // initTerms call removeTermsInAssignedList
		newTerms.remove(assignedTerms);
	}

	if (oldTerms == newTerms)
		return;

	beginResetModel();

	_setTerms(newTerms, _allSortedTerms);
	
	endResetModel();
}

void ListModelTermsAvailable::addAssignedModel(ListModelAssignedInterface *assignedModel)
{
	_assignedModels.push_back(assignedModel);

	connect(this,			&ListModelTermsAvailable::availableTermsReset,			assignedModel,				&ListModelAssignedInterface::availableTermsResetHandler	);
	connect(this,			&ListModelTermsAvailable::variableNamesChanged,			assignedModel,				&ListModelAssignedInterface::sourceVariableNamesChanged	);
	connect(this,			&ListModelTermsAvailable::variablesChanged,				assignedModel,				&ListModelAssignedInterface::sourceVariablesChanged		);
	connect(this,			&ListModelTermsAvailable::variableTypeChanged,		assignedModel,				&ListModelAssignedInterface::sourceVariableTypeChanged	);
	connect(this,			&ListModelTermsAvailable::labelsChanged,				assignedModel,				&ListModelAssignedInterface::sourceLabelsChanged		);
	connect(this,			&ListModelTermsAvailable::labelsReordered,				assignedModel,				&ListModelAssignedInterface::sourceLabelsReordered		);
	connect(this,			&ListModelTermsAvailable::filterChanged,				assignedModel,				&ListModelAssignedInterface::filterChanged				);
	connect(listView(),		&JASPListControl::containsVariablesChanged,				assignedModel->listView(),	&JASPListControl::containsVariablesChanged				);
	connect(listView(),		&JASPListControl::containsInteractionsChanged,			assignedModel->listView(),	&JASPListControl::containsInteractionsChanged			);
}

void ListModelTermsAvailable::removeAssignedModel(ListModelAssignedInterface *assignedModel)
{
	_assignedModels.removeAll(assignedModel);
}

