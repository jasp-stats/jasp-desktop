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
#include "listmodeltermsassigned.h"
#include "../analysis/analysisform.h"
#include <QQmlProperty>

ListModelTermsAvailable::ListModelTermsAvailable(QMLListView* listView)
	: ListModelAvailableInterface(listView)
	, _addEmptyValue(false)
	
{
}

void ListModelTermsAvailable::_resetTerms(const Terms &terms)
{	
	beginResetModel();

	Terms suggested;
	Terms allowed;
	Terms forbidden;

	for (const Term &term : terms)
	{
		if ( ! isAllowed(term))
			forbidden.add(term);
		else if (isSuggested(term))
			suggested.add(term);
		else
			allowed.add(term);
	}
	Terms ordered; // present them in a nice order

	if (_addEmptyValue)
		ordered.add(_emptyValue);
	
	ordered.add(suggested);
	ordered.add(allowed);
	ordered.add(forbidden);

	_allTerms.set(ordered);
	_terms.removeParent();
	_terms.set(ordered);

	_terms.setSortParent(_allTerms);

	endResetModel();	
}

QVariant ListModelTermsAvailable::data(const QModelIndex &index, int role) const
{
	if (_addEmptyValue && role == ListModel::TypeRole && index.row() == 0)
		return QVariant();
	else
		return ListModelAvailableInterface::data(index, role);
}

void ListModelTermsAvailable::resetTermsFromSourceModels()
{
	const QList<QMLListView::SourceType*>& sourceItems = listView()->sourceModels();
	if (sourceItems.size() == 0)
		return;
	
	beginResetModel();
	Terms termsAvailable;
	_termSourceModelMap.empty();
	for (QMLListView::SourceType* sourceItem : sourceItems)
	{
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			Terms terms = sourceModel->terms();
			if (sourceItem->discardModel)
				terms.discardWhatDoesContainTheseComponents(sourceItem->discardModel->terms());
			for (const Term& term : terms)
				_termSourceModelMap[term.asQString()] = sourceModel;
			termsAvailable.add(terms);
		}
	}
	
	setChangedTerms(termsAvailable);
	_resetTerms(termsAvailable);
	removeTermsInAssignedList();
	endResetModel();
}

ListModel *ListModelTermsAvailable::getSourceModelOfTerm(const Term &term)
{
	return _termSourceModelMap[term.asQString()];
}
