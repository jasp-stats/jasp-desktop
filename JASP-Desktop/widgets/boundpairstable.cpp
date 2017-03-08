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

#include "boundpairstable.h"
#include "QDebug"
#include <QHeaderView>

#include <boost/foreach.hpp>

#include <QHBoxLayout>
#include <QLabel>

#include "tablemodelvariablesassigned.h"

using namespace std;

BoundPairsTable::BoundPairsTable(QWidget *parent) :
	TableView(parent)
{
	_variableTypeKey = NULL;

	setModel(new TableModelPairsAssigned(this));
	setSelectionMode(QAbstractItemView::ContiguousSelection);

	this->setDragEnabled(true);
	this->viewport()->setAcceptDrops(true);
	this->setDropIndicatorShown(true);
	this->setDragDropMode(QAbstractItemView::DragDrop);

	horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
	horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
	horizontalHeader()->setSectionsClickable(false);
	horizontalHeader()->hide();

	verticalHeader()->hide();

	setupKey();
}

void BoundPairsTable::bindTo(Option *option)
{
	if (_tableModel != NULL)
		_tableModel->bindTo(option);
}

void BoundPairsTable::notifyDragWasDropped()
{
	if (_tableModel != NULL)
		_tableModel->mimeDataMoved(selectedIndexes());
}

void BoundPairsTable::setModel(QAbstractItemModel *model)
{
	_tableModel = qobject_cast<TableModelPairsAssigned *>(model);

	setupKey();

	QTableView::setModel(model);
}

void BoundPairsTable::resizeEvent(QResizeEvent *e)
{
	QTableView::resizeEvent(e);

	repositionKey();
}

void BoundPairsTable::moveEvent(QMoveEvent *e)
{
	QTableView::moveEvent(e);

	repositionKey();
}

void BoundPairsTable::repositionKey()
{
	_variableTypeKey->move(this->width() - _variableTypeKey->width(), this->height() - _variableTypeKey->height());
}

void BoundPairsTable::setupKey()
{
	if (_variableTypeKey != NULL)
		delete _variableTypeKey;

	_variableTypeKey = new QWidget(this);
	QHBoxLayout *layout = new QHBoxLayout(_variableTypeKey);
	layout->setSpacing(4);
	layout->setContentsMargins(4, 4, 4, 4);
	_variableTypeKey->setLayout(layout);

	if (_tableModel != NULL)
	{
		if (_tableModel->variableTypesSuggested() & Column::ColumnTypeNominal)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-nominal-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_tableModel->variableTypesSuggested() & Column::ColumnTypeOrdinal)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-ordinal-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_tableModel->variableTypesSuggested() & Column::ColumnTypeScale)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-scale-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		_variableTypeKey->resize(_variableTypeKey->sizeHint());
	}

	repositionKey();
	_variableTypeKey->resize(_variableTypeKey->sizeHint());
}
