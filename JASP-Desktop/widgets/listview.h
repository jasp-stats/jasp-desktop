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

#ifndef LISTVIEWWITHFOCUSSIGNAL_H
#define LISTVIEWWITHFOCUSSIGNAL_H

#include <QListView>
#include <QAbstractItemView>

#include "droptarget.h"
#include "tablemodelvariables.h"

class ListView : public QListView, public DropTarget
{
	Q_OBJECT

public:
	explicit ListView(QWidget *parent = 0);

	void setDoubleClickTarget(DropTarget *target);
	virtual void setModel(QAbstractItemModel *model) OVERRIDE;
	virtual void notifyDragWasDropped() OVERRIDE;
	virtual QSize sizeHint() const OVERRIDE;
	virtual QSize minimumSizeHint() const OVERRIDE;
	virtual int itemCount() const;

protected:
	void focusInEvent(QFocusEvent *event) OVERRIDE;
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) OVERRIDE;
	void dropEvent(QDropEvent *event) OVERRIDE;
	bool event(QEvent *e) OVERRIDE;

private slots:
	void doubleClickedHandler(const QModelIndex index);

private:
	DropTarget *_defaultDropTarget;
	TableModel *_listModel;

	
};

#endif // LISTVIEWWITHFOCUSSIGNAL_H
