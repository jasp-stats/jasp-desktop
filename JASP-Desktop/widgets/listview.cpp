#include "listview.h"

#include "draganddrop.h"

#include <QDebug>

ListView::ListView(QWidget *parent) :
	QListView(parent)
{
	_defaultDropTarget = NULL;

	connect(this, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(doubleClickedHandler(QModelIndex)));
}

void ListView::setDoubleClickTarget(DropTarget *target)
{
	_defaultDropTarget = target;
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

void ListView::doubleClickedHandler(const QModelIndex index)
{
	Q_UNUSED(index);

	if (_defaultDropTarget != NULL)
		DragAndDrop::perform(this, _defaultDropTarget);
}
