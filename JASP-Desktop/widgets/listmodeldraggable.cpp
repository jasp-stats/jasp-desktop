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
#include <QDebug>

ListModelDraggable::ListModelDraggable(QMLListView* listView)
	: ListModel(listView)
	, _removeTermsWhenDragged(true)
	, _copyTermsWhenDropped(false)	
{
}

Terms *ListModelDraggable::termsFromIndexes(const QList<int> &indexes) const
{
	Terms* terms = new Terms;
	for (uint index : indexes)
	{
		if (index < _terms.size())
		{
			Term term = _terms.at(index);
			terms->add(term);
		}
	}
	
	return terms;
}

void ListModelDraggable::removeTerms(const QList<int> &indexes)
{
	beginResetModel();

	QList<int> sorted = indexes;
	qSort(sorted.begin(), sorted.end(), qGreater<int>());
	for (const int &index : sorted)
		_terms.remove(index);

	endResetModel();
}

void ListModelDraggable::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	qmlDropMode _dropMode = dropMode();
	if (indexes.length() == 0 || _dropMode == qmlDropMode::None)
		return;	

	beginResetModel();
	Terms* terms = termsFromIndexes(indexes);
	removeTerms(indexes); // Remove first before adding: we cannot add terms that already exist
	for (int index : indexes)
	{
		if (index < dropItemIndex)
			dropItemIndex--;
	}
	Terms* removedTerms = addTerms(terms, dropItemIndex);
	if (removedTerms && removedTerms->size() > 0)
	{
		addTerms(removedTerms);
		delete removedTerms;
	}
	
	delete terms;
	endResetModel();
}

Terms* ListModelDraggable::addTerms(Terms *terms, int dropItemIndex)
{
	Q_UNUSED(dropItemIndex);

	if (terms->size() > 0)
	{
		beginResetModel();
		_terms.add(*terms);
		endResetModel();
	}

	return NULL;
}

bool ListModelDraggable::canAddTerms(Terms *terms) const
{
	for (const Term &term : *terms)
	{
		if ( ! isAllowed(term))
			return false;
	}

	return true;
}

bool ListModelDraggable::isAllowed(const Term &term) const
{
	int variableTypesAllowed = listView()->variableTypesAllowed();
	if (variableTypesAllowed == 0xff) return true;
	if (term.size() > 1) return true;
	
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	Column::ColumnType variableType = (Column::ColumnType)v.toInt();

	return variableType == 0 || variableType & variableTypesAllowed;
}

bool ListModelDraggable::isSuggested(const Term &term) const
{
	int variableTypesSuggested = listView()->variableTypesSuggested();
	if (variableTypesSuggested == 0) return false;
	if (term.size() > 1) return false;
	
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	Column::ColumnType variableType = (Column::ColumnType)v.toInt();

	return variableType == 0 || variableType & variableTypesSuggested;
}
