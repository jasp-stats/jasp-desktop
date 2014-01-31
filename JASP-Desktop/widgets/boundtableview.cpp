
#include "boundtableview.h"

BoundTableView::BoundTableView(QWidget *parent)
	: TableView(parent)
{
	_model = NULL;

	this->setDragEnabled(true);
	this->viewport()->setAcceptDrops(true);
	this->setDropIndicatorShown(true);
	this->setDragDropMode(QAbstractItemView::DragDrop);
}

void BoundTableView::bindTo(Option *option)
{
	if (_model != NULL)
		_model->bindTo(option);
}

void BoundTableView::setModel(QAbstractItemModel *model)
{
	_model = dynamic_cast<BoundModel *>(model);
	TableView::setModel(model);
}
