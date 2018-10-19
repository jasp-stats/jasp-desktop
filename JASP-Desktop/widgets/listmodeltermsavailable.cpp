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
#include "analysis/analysisqmlform.h"
#include "qmllistviewtermsavailable.h"
#include <QQmlProperty>

ListModelTermsAvailable::ListModelTermsAvailable(QMLListView* listView)
	: ListModelAvailableInterface(listView)
{
}

void ListModelTermsAvailable::initTerms(const Terms &terms)
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

	ordered.add(suggested);
	ordered.add(allowed);
	ordered.add(forbidden);

	_allTerms.set(ordered);
	_terms.removeParent();
	_terms.set(ordered);

	_terms.setSortParent(_allTerms);

	endResetModel();	
}

QVariant ListModelTermsAvailable::requestInfo(const Term &term, VariableInfo::InfoType info) const
{
	return VariableInfoConsumer::requestInfo(term, info);
}

void ListModelTermsAvailable::syncTermsChanged(Terms* termsAdded, Terms* termsRemoved)
{
	Q_UNUSED(termsAdded);
	Q_UNUSED(termsRemoved);
	
	resetTermsFromSyncModels();
	emit modelChanged(&_tempAddedTerms, &_tempRemovedTerms);	
}

void ListModelTermsAvailable::_setChangedTerms(const Terms &newTerms)
{
	_tempRemovedTerms.clear();
	_tempAddedTerms.clear();
	for (const Term& term : _allTerms)
	{
		if (!newTerms.contains(term))
			_tempRemovedTerms.add(term);
	}
	
	for (const Term& term : newTerms)
	{
		if (!_allTerms.contains(term))
			_tempAddedTerms.add(term);
	}
}

void ListModelTermsAvailable::resetTermsFromSyncModels()
{
	const QList<ListModel*>& syncModels = listView()->syncModels();
	if (syncModels.size() == 0)
		return;
	
	beginResetModel();
	Terms termsAvailable;
	_termSyncModelMap.empty();
	for (ListModel* syncModel : syncModels)
	{
		const Terms& terms = syncModel->terms();
		for (const Term& term : terms)
			_termSyncModelMap[term.asQString()] = syncModel;
		termsAvailable.add(terms);
	}
	
	_setChangedTerms(termsAvailable);
	initTerms(termsAvailable);
	
	QMLListViewTermsAvailable* qmlAvailableListView = dynamic_cast<QMLListViewTermsAvailable*>(listView());
	if (qmlAvailableListView)
	{
		const QList<ListModelAssignedInterface*>& assignedModels = qmlAvailableListView->assignedModel();	
		for (ListModelAssignedInterface* modelAssign : assignedModels)
		{
			if (!modelAssign->copyTermsWhenDropped())
				_terms.remove(modelAssign->terms());
		}
	}
	endResetModel();
}

ListModel *ListModelTermsAvailable::getSyncModelOfTerm(const Term &term)
{
	return _termSyncModelMap[term.asQString()];
}
