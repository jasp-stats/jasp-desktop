//
// Copyright (C) 2013-2016 University of Amsterdam
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

#include "maintablehorizontalheader.h"

#include <QMouseEvent>
#include <QDebug>

MainTableHorizontalHeader::MainTableHorizontalHeader(QWidget *parent) :
	QHeaderView(Qt::Horizontal, parent)
{
	_menu = new QMenu(this);
	_columnSelected = 0;

	_nominalTextIcon = QIcon(":/icons/variable-nominal-text.svg");
	_nominalIcon = QIcon(":/icons/variable-nominal.svg");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.svg");
	_scaleIcon = QIcon(":/icons/variable-scale.svg");

	_convertToScale = _menu->addAction(_scaleIcon, "", this, SLOT(scaleSelected()));
	_convertToOrdinal = _menu->addAction(_ordinalIcon, "", this, SLOT(ordinalSelected()));
	_convertToNominal = _menu->addAction(_nominalIcon, "", this, SLOT(nominalSelected()));

}

void MainTableHorizontalHeader::mousePressEvent(QMouseEvent *event)
{
	QPoint pos = event->pos();
	int index = logicalIndexAt(pos);

	int itemPos = sectionViewportPosition(index);

	int x = pos.x() - itemPos;

	if (x >= 4 && x <= 24)
	{
		_columnSelected = index;

		QPoint menuPos = this->mapToGlobal(QPoint(itemPos, this->height()));

		_menu->move(menuPos);
		_menu->show();
	}


	QHeaderView::mousePressEvent(event);
}

void MainTableHorizontalHeader::nominalSelected()
{
	emit columnTypeChanged(_columnSelected, Column::ColumnTypeNominal);
}

void MainTableHorizontalHeader::ordinalSelected()
{
	emit columnTypeChanged(_columnSelected, Column::ColumnTypeOrdinal);
}

void MainTableHorizontalHeader::scaleSelected()
{
	emit columnTypeChanged(_columnSelected, Column::ColumnTypeScale);
}
