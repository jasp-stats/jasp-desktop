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

protected:
	void focusInEvent(QFocusEvent *event) OVERRIDE;
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) OVERRIDE;
	void resizeEvent(QResizeEvent *event) OVERRIDE;

	virtual void dropEvent(QDropEvent *event) OVERRIDE;

private slots:
	void doubleClickedHandler(const QModelIndex index);

private:
	DropTarget *_defaultDropTarget;
	TableModel *_tableModel;
};

#endif // TABLEVIEWWIDGET_H
