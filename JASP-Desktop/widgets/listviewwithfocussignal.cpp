#include "listviewwithfocussignal.h"

ListViewWithFocusSignal::ListViewWithFocusSignal(QWidget *parent) :
	QListView(parent)
{
}

void ListViewWithFocusSignal::focusInEvent(QFocusEvent *event)
{
	if (selectedIndexes().length() > 0)
		emit focused();
}

void ListViewWithFocusSignal::selectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
	if (selectedIndexes().length() > 0)
		emit focused();

	QListView::selectionChanged(selected, deselected);
}
