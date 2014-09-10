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
