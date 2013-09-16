#include "availablefieldslistview.h"

#include "boost/foreach.hpp"

#include "availablefields.h"

AvailableFieldsListView::AvailableFieldsListView(QWidget *parent) :
	ListView(parent)
{
	setEditTriggers(QListView::NoEditTriggers);
	this->setSelectionMode(QAbstractItemView::ExtendedSelection);

	this->setDragEnabled(true);
	this->viewport()->setAcceptDrops(true);
	this->setDragDropMode(QAbstractItemView::DragDrop);
}
