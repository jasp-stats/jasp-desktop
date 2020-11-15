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

ListModelTermsAvailable::ListModelTermsAvailable(JASPListControl* listView)
	: ListModelAvailableInterface(listView)
{
}

void ListModelTermsAvailable::sortItems(SortType sortType)
{	
	if (sortType == Sortable::None)
	{
		Terms allowed;
		Terms forbidden;

		for (const Term &term : _allTerms)
		{
			if ( ! isAllowed(term))
				forbidden.add(term);
			else
				allowed.add(term);
		}

		_allTerms.clear();
		_allTerms.add(allowed);
		_allTerms.add(forbidden);
	}

	ListModelAvailableInterface::sortItems(sortType);
}

void ListModelTermsAvailable::resetTermsFromSourceModels(bool updateAssigned)
{
	
	beginResetModel();

	Terms termsAvailable = getSourceTerms();
	
	setChangedTerms(termsAvailable);
	initTerms(termsAvailable);

	endResetModel();

	if (updateAssigned)
		emit allAvailableTermsChanged(&_tempAddedTerms, &_tempRemovedTerms);
}
