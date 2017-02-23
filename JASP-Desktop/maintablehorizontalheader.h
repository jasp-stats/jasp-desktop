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

#ifndef MAINTABLEHORIZONTALHEADER_H
#define MAINTABLEHORIZONTALHEADER_H

#include <QHeaderView>
#include <QMenu>

#include "column.h"
#include "common.h"

class MainTableHorizontalHeader : public QHeaderView
{
	Q_OBJECT

public:
	explicit MainTableHorizontalHeader(QWidget *parent = 0);

signals:
	void columnTypeChanged(int columnIndex, Column::ColumnType newColumnType);
	void columnNamePressed(int columnIndex);

public slots:

protected:
	virtual void mousePressEvent(QMouseEvent *event) OVERRIDE;
	virtual void mouseMoveEvent(QMouseEvent *event) OVERRIDE;

private slots:
	void nominalSelected();
	void ordinalSelected();
	void scaleSelected();

private:
	int _columnSelected;

	QMenu *_menu;

	QIcon _nominalTextIcon;
	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;

	QAction *_convertToNominal;
	QAction *_convertToOrdinal;
	QAction *_convertToScale;

};

#endif // MAINTABLEHORIZONTALHEADER_H
