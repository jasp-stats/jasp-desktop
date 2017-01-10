//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "listview.h"

#include "draganddrop.h"

#include <QDropEvent>
#include <QScrollBar>

ListView::ListView(QWidget *parent) :
	QListView(parent)
{
	_defaultDropTarget = NULL;
	_listModel = NULL;

	this->setLayoutMode(QListView::Batched);
	this->setBatchSize(25);

	connect(this, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(doubleClickedHandler(QModelIndex)));

	setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
}

void ListView::setDoubleClickTarget(DropTarget *target)
{
	_defaultDropTarget = target;
}

void ListView::setModel(QAbstractItemModel *model)
{
	_listModel = qobject_cast<TableModel*>(model);

	QListView::setModel(model);
}

void ListView::notifyDragWasDropped()
{
	if (_listModel != NULL)
		_listModel->mimeDataMoved(selectedIndexes());
}

void ListView::focusInEvent(QFocusEvent *event)
{
	QListView::focusInEvent(event);
	focused();
}

void ListView::selectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
	QListView::selectionChanged(selected, deselected);
	selectionUpdated();
}

void ListView::dropEvent(QDropEvent *event)
{
	QListView::dropEvent(event);

	if (event->isAccepted() && event->dropAction() == Qt::MoveAction)
	{
		QObject *source = event->source();
		DropTarget *draggedFrom = dynamic_cast<DropTarget*>(source);
		if (draggedFrom != NULL && event->source() != this)
			draggedFrom->notifyDragWasDropped();
	}
}

bool ListView::event(QEvent *e)
{
	if (e->type() == QEvent::Wheel)
	{
		bool ret = QListView::event(e);

		QScrollBar *scrollBar = this->verticalScrollBar();
		if (scrollBar != NULL)
		{
			// eat the mouse wheel event if scrolling (return true)
			// otherwise it propogates, and scrolls the parent as well

			if (scrollBar->value() < scrollBar->maximum() && scrollBar->value() > scrollBar->minimum())
				return true;
		}

		return ret;
	}

	return QListView::event(e);
}

void ListView::doubleClickedHandler(const QModelIndex index)
{
	Q_UNUSED(index);

	if (_defaultDropTarget != NULL)
		DragAndDrop::perform(this, _defaultDropTarget);
}

QSize ListView::sizeHint() const
{
	static int width = -1;

	if (width == -1)
		width = this->fontMetrics().width("XXXXXXXXXXXXXXXXXXXX");


	QSize sizeHint = QListView::sizeHint();
	sizeHint.setWidth(width);

	return sizeHint;
}

QSize ListView::minimumSizeHint() const
{
	static int width = -1;

	if (width == -1)
		width = this->fontMetrics().width("XXXXXXXXXXXXXXXXXXXX");

	QSize sizeHint = QListView::minimumSizeHint();
	sizeHint.setWidth(width);

	return sizeHint;
}

int ListView::itemCount() const
{
	return -1;
}
