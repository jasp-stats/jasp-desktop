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

void ListModelTermsAvailable::resetTermsFromSources(bool updateAssigned)
{
	
	beginResetModel();

	Terms termsAvailable = getSourceTerms();
	Terms removedTerms, addedTerms;
	
	for (const Term& term : _allTerms)
		if (!termsAvailable.contains(term))
			removedTerms.add(term);

	for (const Term& term : termsAvailable)
		if (!_allTerms.contains(term))
			addedTerms.add(term);

	initTerms(termsAvailable);

	endResetModel();

	if (updateAssigned)
		emit availableTermsReset(addedTerms, removedTerms);
}
