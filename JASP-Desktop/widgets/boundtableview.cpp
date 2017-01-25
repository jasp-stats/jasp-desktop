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

void BoundTableView::unbind()
{
	if (_model != NULL)
		_model->unbind();
}

void BoundTableView::setModel(QAbstractItemModel *model)
{
	_model = dynamic_cast<BoundModel *>(model);
	TableView::setModel(model);
}
