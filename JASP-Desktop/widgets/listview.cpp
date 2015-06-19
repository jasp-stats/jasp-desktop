#include "listview.h"

#include "draganddrop.h"

#include <QDropEvent>

ListView::ListView(QWidget *parent) :
	QListView(parent)
{
	_defaultDropTarget = NULL;
	_listModel = NULL;

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

void ListView::doubleClickedHandler(const QModelIndex index)
{
	Q_UNUSED(index);

	if (_defaultDropTarget != NULL)
		DragAndDrop::perform(this, _defaultDropTarget);
}
