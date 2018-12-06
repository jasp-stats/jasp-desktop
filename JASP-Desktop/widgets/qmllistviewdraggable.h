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
#include "analysis/analysisqmldefines.h"

class ListModelDraggable;

class QMLListViewDraggable : public QMLListView
{
	Q_OBJECT

public:
	QMLListViewDraggable(QQuickItem* item, AnalysisQMLForm* form);
	
	virtual void setUp() OVERRIDE;
	
	ListModelDraggable* draggableModel() const		{ return _draggableModel; }

protected:
	void setDropMode(qmlDropMode dropMode);
	
	ListModelDraggable* _draggableModel;
	
private slots:
	void moveItemsDelayedHandler();
	void itemDoubleClickedHandler(int index);
	void itemsDroppedHandler(QVariant indexes, QVariant vdropList, int dropItemIndex);	
	
private:
	QList<int> _tempIndexes;
	ListModelDraggable* _tempDropModel;
	int _tempDropItemIndex;
	
	void _moveItems(QList<int> &indexes, ListModelDraggable* dropModel, int dropItemIndex);	
};

#endif // QMLLISTVIEWDRAGGABLE_H
