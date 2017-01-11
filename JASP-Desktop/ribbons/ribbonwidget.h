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

#ifndef RIBBONWIDGET_H
#define RIBBONWIDGET_H

#include <QWidget>
#include "widgets/ribbonbutton.h"

class RibbonWidget : public QWidget
{
	Q_OBJECT
public:
	explicit RibbonWidget(QWidget *parent = 0);
	void addRibbonButton(RibbonButton *button);

signals:
	void itemSelected(QString itemName);

public slots:
	void setDataSetLoaded(bool loaded);

protected slots:
	void itemSelected();

private:

	QList<RibbonButton*> _buttons;

};

#endif // RIBBONWIDGET_H
