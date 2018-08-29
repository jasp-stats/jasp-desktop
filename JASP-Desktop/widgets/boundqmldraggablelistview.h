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

#ifndef BOUNDQMLDRAGGABLELISTVIEW_H
#define BOUNDQMLDRAGGABLELISTVIEW_H

#include "boundqmltableview.h"
#include "listmodeltermsassignedinterface.h"

class BoundQMLDraggableListView : public BoundQMLTableView
{
public:
	explicit BoundQMLDraggableListView(QQuickItem* item, AnalysisQMLForm* form) 
		: BoundQMLTableView(item, form) 
	{ _needsSyncModels = false; }
	
	virtual ListModelTermsAssignedInterface* targetModel() { return _targetModel; }
	virtual ListModelTermsAvailableInterface* sourceModel() { return _sourceModel; }

	virtual void setUp() OVERRIDE;

protected:
	ListModelTermsAvailableInterface* _sourceModel;
	ListModelTermsAssignedInterface* _targetModel;
};

#endif // BOUNDQMLDRAGGABLELISTVIEW_H
