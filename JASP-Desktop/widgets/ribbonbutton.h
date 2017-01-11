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

#ifndef RIBBONBUTTON_H
#define RIBBONBUTTON_H

#include <QToolButton>
#include <QString>
#include <QMouseEvent>

#include "common.h"

class RibbonButton : public QToolButton
{
    Q_OBJECT
public:
	explicit RibbonButton(QWidget *parent = 0);
	void setDataSetNotNeeded();
	bool isDataSetNeeded() const;

public slots:
	void notifyMouseOut();
	void notifyMouseOver();

protected:
	virtual void enterEvent(QEvent *event) OVERRIDE;
	virtual void mousePressEvent(QMouseEvent *event) OVERRIDE;

private:

	bool _dataSetNeeded;

	bool _mouseOver;

	QString _mouseOutSS;
	QString _mouseOverSS;

	bool _connectedToMenu;
};

#endif // RIBBONBUTTON_H
