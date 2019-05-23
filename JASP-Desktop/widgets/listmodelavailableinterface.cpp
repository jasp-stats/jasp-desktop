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
#include "qmllistviewtermsavailable.h"


void ListModelAvailableInterface::initTerms(const Terms &terms)
{
	beginResetModel();
	
	ListModelDraggable::initTerms(terms);
	_allTerms = _terms;
	_terms.setSortParent(_allTerms);
	removeTermsInAssignedList();
	
	endResetModel();
}

QVariant ListModelAvailableInterface::requestInfo(const Term &term, VariableInfo::InfoType info) const
{
	return VariableInfoConsumer::requestInfo(term, info);
}

void ListModelAvailableInterface::sourceTermsChanged(Terms* termsAdded, Terms* termsRemoved)
{
	Q_UNUSED(termsAdded);
	Q_UNUSED(termsRemoved);
	
	resetTermsFromSourceModels();
}


void ListModelAvailableInterface::setChangedTerms(const Terms &newTerms)
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

void ListModelAvailableInterface::removeTermsInAssignedList()
{
	beginResetModel();
	
	_terms = _allTerms;
	_terms.setSortParent(_allTerms);
	
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

