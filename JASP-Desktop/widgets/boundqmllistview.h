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

#ifndef BOUNDQMLLISTVIEW_H
#define BOUNDQMLLISTVIEW_H

#include "analysis/boundqmlitem.h"
#include "listmodelassigned.h"

class BoundQMLListView : public BoundQMLItem
{
public:
	explicit BoundQMLListView(QQuickItem* item, AnalysisQMLForm* form) : BoundQMLItem(item, form) {	}
	
	virtual ListModelAssigned* targetModel() { return _targetModel; }
	virtual ListModelAvailable* sourceModel() { return _sourceModel; }

	virtual void setUp() OVERRIDE;

protected:
	ListModelAvailable* _sourceModel;
	ListModelAssigned* _targetModel;
};

#endif // BOUNDQMLLISTVIEW_H
