
#include "boundtableview.h"

BoundTableView::BoundTableView(QWidget *parent)
	: TableView(parent)
{
	_model = NULL;
}

void BoundTableView::bindTo(Option *option)
{
	if (_model != NULL)
		_model->bindTo(option);
}

void BoundTableView::setModel(QAbstractItemModel *model)
{
	_model = qobject_cast<TableModelVariablesOptions *>(model);
	TableView::setModel(model);
}
