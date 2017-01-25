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

#ifndef TABLEVIEWWIDGET_H
#define TABLEVIEWWIDGET_H

#include <QTableView>

#include "droptarget.h"
#include "tablemodel.h"

#include "common.h"

class TableView : public QTableView, public DropTarget
{
	Q_OBJECT
public:
	explicit TableView(QWidget *parent = 0);

	virtual void setModel(QAbstractItemModel *model) OVERRIDE;

	void setDoubleClickTarget(DropTarget *target);
	virtual void notifyDragWasDropped() OVERRIDE;
	virtual QSize sizeHint() const OVERRIDE;

protected:
	void focusInEvent(QFocusEvent *event) OVERRIDE;
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) OVERRIDE;
	void resizeEvent(QResizeEvent *event) OVERRIDE;

	virtual void dropEvent(QDropEvent *event) OVERRIDE;

	void virtual mousePressEvent(QMouseEvent *event) OVERRIDE;

private slots:
	void doubleClickedHandler(const QModelIndex index);

private:
	DropTarget *_defaultDropTarget;
	TableModel *_tableModel;
};

#endif // TABLEVIEWWIDGET_H
