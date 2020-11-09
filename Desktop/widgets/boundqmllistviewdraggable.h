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

#ifndef BOUNDQMLLISTVIEWDRAGGABLE_H
#define BOUNDQMLLISTVIEWDRAGGABLE_H

#include "analysis/boundqmlitem.h"
#include "qmllistviewdraggable.h"
#include "analysis/options/terms.h"

class ListModelAvailableInterface;
class ListModelAssignedInterface;

class BoundQMLListViewDraggable : public QMLListViewDraggable, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLListViewDraggable(JASPControl* item);
	
	void						setUp() override;
	ListModelAssignedInterface* assignedModel();
	
protected:
	ListModelAvailableInterface*		_availableModel;
	
private:
	Terms								_tempTerms;
	
};

#endif // BOUNDQMLLISTVIEWDRAGGABLE_H
