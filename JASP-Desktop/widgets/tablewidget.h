//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef TABLEWIDGET_H
#define TABLEWIDGET_H

#include <QWidget>


namespace Ui {
class TableWidget;
}

class TableWidget : public QWidget
{
	Q_OBJECT

public:
	explicit TableWidget(QWidget *parent = 0);
	~TableWidget();

private:
	Ui::TableWidget *ui;
};

#endif // TABLEWIDGET_H
