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

#include "listmodeltermsassigned.h"
#include "listmodeltermsavailable.h"
#include <QTimer>
#include <QDebug>

using namespace std;

ListModelTermsAssigned::ListModelTermsAssigned(QMLListView* listView, bool onlyOneTerm)
	: ListModelAssignedInterface(listView)
	, _onlyOneTerm(onlyOneTerm)
{
}

void ListModelTermsAssigned::initTerms(const Terms &terms)
{
	beginResetModel();
	_terms.set(terms);
	endResetModel();
	
	if (source() != NULL)
	{
		if (!_copyTermsWhenDropped)
			source()->removeAssignedTerms(_terms);
	}
}

void ListModelTermsAssigned::availableTermsChanged(Terms* termsAdded, Terms* termsRemoved)
{
	// Only remove the terms that are not available anymore
	Q_UNUSED(termsAdded);
	
	if (termsRemoved && termsRemoved->size() > 0)
	{
		beginResetModel();
		_terms.remove(*termsRemoved);
		endResetModel();
		_tempTermsToRemove.set(termsRemoved);
		emit modelChanged(NULL, &_tempTermsToRemove);
	}	
}

bool ListModelTermsAssigned::canAddTerms(Terms *terms) const
{
	if ( ! ListModelDraggable::canAddTerms(terms))
		return false;

	if (_onlyOneTerm && terms->size() != 1)
		return false;

	return true;
}

Terms* ListModelTermsAssigned::addTerms(Terms *terms, int dropItemIndex)
{
	Terms newTerms;
	Terms *toSendBack = new Terms;
	beginResetModel();

	if (_onlyOneTerm)
	{
		if (terms->size() > 0)
			newTerms.add(terms->at(0));

		if (_terms.size() > 0)
			toSendBack->set(_terms);
	}
	else if (dropItemIndex >= 0 && dropItemIndex < static_cast<int>(_terms.size()))
	{
		newTerms.set(_terms);
		newTerms.insert(dropItemIndex, *terms);
	}
	else
	{
		newTerms.set(_terms);
		newTerms.add(*terms);
	}
	
	_terms.set(newTerms);
	endResetModel();	

	emit modelChanged(terms, toSendBack);
	
	return toSendBack;
}

void ListModelTermsAssigned::removeTerms(const QList<int> &indices)
{
	beginResetModel();
	_tempTermsToRemove.clear();
	for (const int &index : indices)
		_tempTermsToRemove.add(_terms.at(index));
	
	_terms.remove(_tempTermsToRemove);
	endResetModel();	

	emit modelChanged(NULL, &_tempTermsToRemove);	
}
