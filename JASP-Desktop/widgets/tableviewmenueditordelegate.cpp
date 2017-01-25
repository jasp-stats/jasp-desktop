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

#include "tableviewmenueditordelegate.h"

#include <QDebug>

TableViewMenuEditorDelegate::TableViewMenuEditorDelegate(QObject *parent) :
	QStyledItemDelegate(parent)
{
}

QWidget *TableViewMenuEditorDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	QVariant v = index.data(Qt::EditRole);

	if (v.canConvert<QList<QVariant> >())
	{
		QList<QVariant> value = qvariant_cast<QList<QVariant> >(v);
		QString selected = value.first().toString();
		QStringList items = value.last().toStringList();

		TableViewMenuEditor *editor = new TableViewMenuEditor(parent);

		editor->setMenuContent(items, selected);
		connect(editor, SIGNAL(editingFinished()), this, SLOT(editingFinished()));

		return editor;
	}
	else
	{
		return QStyledItemDelegate::createEditor(parent, option, index);
	}
}

void TableViewMenuEditorDelegate::setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const
{
	TableViewMenuEditor *menuEditor = qobject_cast<TableViewMenuEditor *>(editor);
	if (menuEditor != NULL)
	{
		if (menuEditor->selected() != "")
			model->setData(index, menuEditor->selected(), Qt::DisplayRole);
	}
	else
	{
		QStyledItemDelegate::setModelData(editor, model, index);
	}
}

void TableViewMenuEditorDelegate::editingFinished()
{
	TableViewMenuEditor *editor = qobject_cast<TableViewMenuEditor *>(sender());

	emit commitData(editor);
	emit closeEditor(editor);
}
