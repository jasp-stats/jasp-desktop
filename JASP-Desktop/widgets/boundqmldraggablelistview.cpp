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

#include "boundqmldraggablelistview.h"
#include "analysis/analysisqmlform.h"

void BoundQMLDraggableListView::setUp()
{
	BoundQMLTableView::setUp();
	
	ListModelTermsAssignedInterface* assignedModel = targetModel();
	assignedModel->setUp();
	
	ListModelDraggableTerms* sourceModel = _form->getRelatedModel(_item);
	if (!sourceModel)
		addError(QString::fromLatin1("Cannot find source ListView for item ") + name());
	else
	{
		_sourceModel = dynamic_cast<ListModelTermsAvailableInterface*>(sourceModel);
		if (!_sourceModel)
			addError(QString::fromLatin1("Wrong kind of source ListView for item ") + name());
		else
		{
			_sourceModel->setUp();
			assignedModel->setSource(_sourceModel);
			connect(_sourceModel, &ListModelTermsAvailableInterface::termsChanged, _targetModel, &ListModelTermsAssignedInterface::availableTermsChanged);			
		}
	}
}
