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


using namespace std;

ListModelTermsAssigned::ListModelTermsAssigned(QMLListView* listView, int maxRows)
	: ListModelAssignedInterface(listView)
	, _maxRows(maxRows)
{
}

void ListModelTermsAssigned::initTerms(const Terms &terms, const RowControlsOptions& allOptionsMap)
{
	ListModelAssignedInterface::initTerms(terms, allOptionsMap);
	
	if (source() != nullptr)
	{
		if (!_copyTermsWhenDropped)
			source()->removeTermsInAssignedList();
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
		emit modelChanged(nullptr, &_tempTermsToRemove);
	}	
}

bool ListModelTermsAssigned::canAddTerms(Terms *terms) const
{
	if ( ! ListModelDraggable::canAddTerms(terms))
		return false;

	if (_maxRows >= 0 && int(terms->size()) > _maxRows)
		return false;

	return true;
}

Terms* ListModelTermsAssigned::addTerms(Terms *terms, int dropItemIndex, const QString&)
{
	Terms newTerms;
	Terms *toSendBack = new Terms;
	beginResetModel();

	newTerms.set(_terms);
	if (dropItemIndex < 0 && _maxRows == 1)
		dropItemIndex = 0; // for single row, per default replace old item by new one.
	if (dropItemIndex >= 0 && dropItemIndex < int(_terms.size()))
		newTerms.insert(dropItemIndex, *terms);
	else
		newTerms.add(*terms);

	size_t maxRows = size_t(_maxRows);
	if (newTerms.size() > maxRows)
	{
		for (size_t i = maxRows; i < newTerms.size(); i++)
			toSendBack->add(newTerms.at(i));
		newTerms.remove(maxRows, newTerms.size() - maxRows);
	}

	_terms.set(newTerms);

	endResetModel();	

	emit modelChanged(terms, toSendBack);
	
	return toSendBack;
}
