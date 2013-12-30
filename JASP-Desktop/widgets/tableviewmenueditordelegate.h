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
