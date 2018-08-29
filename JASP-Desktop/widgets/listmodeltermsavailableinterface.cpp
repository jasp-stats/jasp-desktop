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

#include "listmodeltermsavailableinterface.h"
#include "listmodeltermsassignedinterface.h"

void ListModelTermsAvailableInterface::removeAssignedTerms(const Terms &terms)
{
	beginResetModel();
	_terms.remove(terms);
	endResetModel();
}

void ListModelTermsAvailableInterface::setTermsAreNotVariables() 
{
	ListModelDraggableTerms::setTermsAreNotVariables();
	for (ListModelTermsAssignedInterface* model : _assignedModels)
		model->setTermsAreNotVariables();
}
