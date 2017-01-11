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

#ifndef INFOPOPUP_H
#define INFOPOPUP_H

#include <QWidget>
#include <QPainterPath>
#include <QPen>

#include "common.h"

class InfoPopup : public QWidget
{
	Q_OBJECT
public:
	explicit InfoPopup(QWidget *parent = 0);

	enum PopupDirection { BottomRight, BottomLeft, TopLeft };

	void setDirection(PopupDirection direction);

	int pointLength();
	int pointWidth();
	int pointOffset();
	
signals:

protected:
	virtual void paintEvent(QPaintEvent *event) OVERRIDE;
	
public slots:

private:
	PopupDirection _popupDirection;
	QBrush _backgroundBrush;
	QPen _borderPen;

	int _pointLength;
	int _pointWidth;
	int _pointOffset;
	int _borderWidth;
	
};

#endif // INFOPOPUP_H
