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

#ifndef TABLEVIEWMENUEDITOR_H
#define TABLEVIEWMENUEDITOR_H

#include <QWidget>
#include <QMenu>

#include "common.h"

class TableViewMenuEditor : public QWidget
{
	Q_OBJECT
public:
	explicit TableViewMenuEditor(QWidget *parent = 0);
	QString selected();

	void setMenuContent(QStringList menuItems, QString selected);

signals:
	void editingFinished();

protected:
	virtual void showEvent(QShowEvent *) OVERRIDE;
	virtual void paintEvent(QPaintEvent *event) OVERRIDE;

private slots:
	void aboutToHide();

private:

	QMenu *_menu;
	QString _selected;

};

#endif // TABLEVIEWMENUEDITOR_H
