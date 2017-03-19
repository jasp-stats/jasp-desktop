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

#include "infopopup.h"

#include <QPainter>
#include <QPaintEvent>
#include <QPainterPath>
#include <QColor>
#include <QGridLayout>
#include <QBrush>
#include <QPalette>


InfoPopup::InfoPopup(QWidget *parent) :
	QWidget(parent)
{
	_pointLength = 20;
	_pointWidth = 20;
	_pointOffset = 30;
	_borderWidth = 4;

	QGridLayout *layout = new QGridLayout(this);
	setLayout(layout);

	setDirection(TopLeft);

	setAttribute(Qt::WA_TransparentForMouseEvents);

	setMinimumHeight(_pointLength);
	setMinimumWidth(_pointWidth);

	_backgroundBrush = QBrush(QColor(255, 192, 192));
	_borderPen = QPen(QColor(255, 127, 127), 2);
}

void InfoPopup::setDirection(InfoPopup::PopupDirection direction)
{
	_popupDirection = direction;

	if (_popupDirection == TopLeft)
		layout()->setContentsMargins(-1, 9 + _pointLength, -1, -1);
	else
		layout()->setContentsMargins(-1, -1, -1, 9 + _pointLength);
}

int InfoPopup::pointLength()
{
	return _pointLength;
}

int InfoPopup::pointWidth()
{
	return _pointWidth;
}

int InfoPopup::pointOffset()
{
	return _pointOffset;
}


void InfoPopup::paintEvent(QPaintEvent *event)
{
	(void)event;

	QPainter painter(this);

	QPainterPath shape = QPainterPath();

	int x = _borderWidth / 2;
	int y = _borderWidth / 2;
	int w = width() - _borderWidth;
	int h = height() - _borderWidth;

	if (_popupDirection == TopLeft)
	{
		shape.moveTo(x, _pointLength);
		shape.lineTo(_pointOffset, _pointLength);
		shape.lineTo(_pointOffset + _pointWidth / 2, y);
		shape.lineTo(_pointOffset + _pointWidth, _pointLength);
		shape.lineTo(w, _pointLength);
		shape.lineTo(w, h);
		shape.lineTo(x, h);
		shape.closeSubpath();
	}
	else if (_popupDirection == BottomLeft)
	{
		shape.moveTo(x, y);
		shape.lineTo(w, y);
		shape.lineTo(w, h - _pointLength);
		shape.lineTo(x + _pointOffset + _pointWidth, h - _pointLength);
		shape.lineTo(x + _pointOffset + _pointWidth / 2, h);
		shape.lineTo(x + _pointOffset, h - _pointLength);
		shape.lineTo(x, h - _pointLength);
		shape.closeSubpath();
	}
	else if (_popupDirection == BottomRight)
	{
		shape.moveTo(x, y);
		shape.lineTo(w, y);
		shape.lineTo(w, h - _pointLength);
		shape.lineTo(w - _pointOffset, h - _pointLength);
		shape.lineTo(w - _pointOffset - _pointWidth / 2, h);
		shape.lineTo(w - _pointOffset - _pointWidth, h - _pointLength);
		shape.lineTo(x, h - _pointLength);
		shape.closeSubpath();
	}

	painter.fillPath(shape, _backgroundBrush);

	painter.setPen(_borderPen);
	painter.drawPath(shape);

	painter.end();
}
