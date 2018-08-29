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

#ifndef LISTMODELTERMSASSIGNEDINTERFACE_H
#define LISTMODELTERMSASSIGNEDINTERFACE_H

#include "listmodeldraggableterms.h"
#include "boundmodel.h"

#include "listmodeltermsavailableinterface.h"

class ListModelTermsAssignedInterface : public ListModelDraggableTerms
{
public:
	ListModelTermsAssignedInterface(AnalysisQMLForm *form, QQuickItem* item) 
		: ListModelDraggableTerms(form, item)
		, _source(NULL) 
	{}
	
	virtual void setSource(ListModelTermsAvailableInterface *source) {
		_source = source;
		_source->addAssignedModel(this);
		if (!_source->areTermsVariables())
			setTermsAreNotVariables();
	}

public slots:
	virtual void availableTermsChanged(Terms *termsAdded, Terms *termsRemoved) {}

protected:
	ListModelTermsAvailableInterface* _source;
};

#endif // LISTMODELTERMSASSIGNEDINTERFACE_H
