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

#include "boundlistview.h"

#include "boost/bind.hpp"
#include "boost/foreach.hpp"

#include <vector>
#include <string>

#include "options/optionvariables.h"

#include "widgets/boundmodel.h"
#include "tableviewmenueditordelegate.h"

#include <QLabel>
#include <QHBoxLayout>

using namespace std;

BoundListView::BoundListView(QWidget *parent)
	: ListView(parent)
{
	_variablesListModel = NULL;

	setEditTriggers(QListView::NoEditTriggers);
	setSelectionMode(QAbstractItemView::ExtendedSelection);

	this->setDragEnabled(true);
	this->viewport()->setAcceptDrops(true);
	this->setDropIndicatorShown(true);
	this->setDragDropMode(QAbstractItemView::DragDrop);

	_variableTypeKey = new QWidget(this);
	QHBoxLayout *layout = new QHBoxLayout(_variableTypeKey);
	layout->setSpacing(4);
	layout->setContentsMargins(4, 4, 4, 4);
	_variableTypeKey->setLayout(layout);
	_variableTypeKey->resize(_variableTypeKey->sizeHint());

	this->setItemDelegate(new TableViewMenuEditorDelegate(this));
}

void BoundListView::setModel(QAbstractItemModel *model)
{
	_variablesListModel = qobject_cast<TableModelVariablesAssigned *>(model);

	if (_variablesListModel != NULL)
	{
		if (_variablesListModel->variableTypesSuggested() & Column::ColumnTypeNominal)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-nominal-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_variablesListModel->variableTypesSuggested() & Column::ColumnTypeOrdinal)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-ordinal-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_variablesListModel->variableTypesSuggested() & Column::ColumnTypeScale)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-scale-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		_variableTypeKey->resize(_variableTypeKey->sizeHint());

		repositionKey();
	}

	ListView::setModel(model);
}

void BoundListView::bindTo(Option *option)
{
	BoundModel *model = dynamic_cast<BoundModel *>(this->model());
	if (model != NULL)
		model->bindTo(option);
}

void BoundListView::unbind()
{
	BoundModel *model = dynamic_cast<BoundModel *>(this->model());
	if (model != NULL)
		model->unbind();
}

void BoundListView::resizeEvent(QResizeEvent *e)
{
	ListView::resizeEvent(e);

	repositionKey();
}

void BoundListView::moveEvent(QMoveEvent *e)
{
	ListView::moveEvent(e);

	repositionKey();
}

void BoundListView::repositionKey()
{
	_variableTypeKey->move(this->width() - _variableTypeKey->width(), this->height() - _variableTypeKey->height());
}
