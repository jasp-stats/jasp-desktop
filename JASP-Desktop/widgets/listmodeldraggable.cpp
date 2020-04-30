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

#include "listmodeldraggable.h"
#include "analysis/analysisform.h"

ListModelDraggable::ListModelDraggable(QMLListView* listView)
	: ListModel(listView)
	, _copyTermsWhenDropped(false)	
{
	_allowAnalysisOwnComputedColumns = listView->getItemProperty("allowAnalysisOwnComputedColumns").toBool();
	_addNewAvailableTermsToAssignedModel = listView->getItemProperty("addAvailableVariablesToAssigned").toBool();
}

Terms ListModelDraggable::termsFromIndexes(const QList<int> &indexes) const
{
	Terms result;
	const Terms& myTerms = terms();
	for (int index : indexes)
	{
		size_t index_t = size_t(index);
		if (index_t < myTerms.size())
		{
			const Term& term = myTerms.at(index_t);
			result.add(term);
		}
	}
	
	return result;
}

void ListModelDraggable::removeTerms(const QList<int> &indices)
{
	beginResetModel();

	_tempTermsToRemove.clear();
	for (int index : indices)
		if (index < int(_terms.size()))
			_tempTermsToRemove.add(_terms.at(size_t(index)));

	_terms.remove(_tempTermsToRemove);

	endResetModel();

	emit modelChanged(nullptr, &_tempTermsToRemove);
}


void ListModelDraggable::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	JASPControlBase::DropMode _dropMode = dropMode();
	if (indexes.length() == 0 || _dropMode == JASPControlBase::DropMode::DropNone)
		return;	

	beginResetModel();
	Terms terms = termsFromIndexes(indexes);
	removeTerms(indexes); // Remove first before adding: we cannot add terms that already exist
	for (int index : indexes)
	{
		if (index < dropItemIndex)
			dropItemIndex--;
	}
	Terms removedTerms = addTerms(terms, dropItemIndex);
	if (removedTerms.size() > 0)
	{
		addTerms(removedTerms);
	}
	
	endResetModel();
}

Terms ListModelDraggable::addTerms(const Terms& terms, int dropItemIndex, JASPControlBase::AssignType)
{
	Q_UNUSED(dropItemIndex);

	if (terms.size() > 0)
	{
		beginResetModel();
		_terms.add(terms);
		endResetModel();

		_tempTermsToAdd = terms;
		emit modelChanged(&_tempTermsToAdd, nullptr);
	}

	return nullptr;
}

Terms ListModelDraggable::canAddTerms(const Terms& terms) const
{
	Terms result;
	for (const Term &term : terms)
	{
		if (isAllowed(term))
			result.add(term);
	}

	return result;
}

bool ListModelDraggable::isAllowed(const Term &term) const
{
	if (!_allowAnalysisOwnComputedColumns)
	{
		if (listView()->form()->isOwnComputedColumn(term.asQString()))
			return false;
	}

	int variableTypesAllowed = listView()->variableTypesAllowed();

	if (variableTypesAllowed == 0xff || term.size() > 1)
		return true;
	
	QVariant	v				= requestInfo(term, VariableInfo::VariableType);
	int			variableType	= v.toInt();

	return variableType == 0 || variableType & variableTypesAllowed;
}
