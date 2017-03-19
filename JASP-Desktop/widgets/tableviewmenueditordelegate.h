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

#ifndef TABLEVIEWCOMBOBOXDELEGATE_H
#define TABLEVIEWCOMBOBOXDELEGATE_H

#include <QStyledItemDelegate>

#include "common.h"
#include <QMenu>
#include <QLabel>
#include "tableviewmenueditor.h"

class TableViewMenuEditorDelegate : public QStyledItemDelegate
{
	Q_OBJECT
public:
	explicit TableViewMenuEditorDelegate(QObject *parent = 0);

	virtual QWidget *createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const OVERRIDE;
	virtual void setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const OVERRIDE;

private slots:
	void editingFinished();

};

#endif // TABLEVIEWCOMBOBOXDELEGATE_H
