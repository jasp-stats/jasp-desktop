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

#ifndef QMLLISTVIEWDRAGGABLE_H
#define QMLLISTVIEWDRAGGABLE_H

#include "qmllistview.h"
#include <QVariant>
#include <QList>
#include "analysis/jaspcontrolbase.h"

class ListModelDraggable;

class QMLListViewDraggable : public QMLListView
{
	Q_OBJECT

public:
	QMLListViewDraggable(JASPControlBase* item);
	
	void setUp() override;
	
	ListModelDraggable* draggableModel() const		{ return _draggableModel; }
	
	void moveItems(QList<int> &indexes, ListModelDraggable* dropModel, int dropItemIndex = -1, JASPControlBase::AssignType assignOption = JASPControlBase::AssignType::AssignDefault);

protected:
	ListModelDraggable* _draggableModel;
	
private slots:
	void moveItemsDelayedHandler();
	void itemDoubleClickedHandler(int index);
	void itemsDroppedHandler(QVariant indexes, QVariant vdropList, int dropItemIndex, int assignOption);
	
private:
	ListModelDraggable	*			_tempDropModel = nullptr;
	QList<int>						_tempIndexes;
	int								_tempDropItemIndex;
	JASPControlBase::AssignType		_tempAssignOption = JASPControlBase::AssignType::AssignDefault;
	
};

#endif // QMLLISTVIEWDRAGGABLE_H
