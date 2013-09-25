
#include "tableview.h"

#include <QHeaderView>
#include <QResizeEvent>

#include "draganddrop.h"

#include <QDebug>

TableView::TableView(QWidget *parent) :
	QTableView(parent)
{
	_defaultDropTarget = NULL;

	connect(this, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(doubleClickedHandler(QModelIndex)));

	horizontalHeader()->hide();
	verticalHeader()->hide();

	setShowGrid(false);
	setSelectionBehavior(QAbstractItemView::SelectRows);

	verticalHeader()->setDefaultSectionSize(verticalHeader()->fontMetrics().height() + 2);
}

void TableView::setDoubleClickTarget(DropTarget *target)
{
	_defaultDropTarget = target;
}

void TableView::focusInEvent(QFocusEvent *event)
{
	QTableView::focusInEvent(event);
	focused();
}

void TableView::selectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
	QTableView::selectionChanged(selected, deselected);
	selectionUpdated();
}

void TableView::resizeEvent(QResizeEvent *event)
{
	int columnCount = model()->columnCount();
	int width = event->size().width();

	if (columnCount == 1)
	{
		setColumnWidth(0, width);
	}
	else if (columnCount == 2)
	{
		setColumnWidth(0, width - 50);
		setColumnWidth(1, 50);
	}

	QTableView::resizeEvent(event);
}

void TableView::doubleClickedHandler(const QModelIndex index)
{
	Q_UNUSED(index);

	if (_defaultDropTarget != NULL)
		DragAndDrop::perform(this, _defaultDropTarget);
}
