#ifndef TABLEVIEWWIDGET_H
#define TABLEVIEWWIDGET_H

#include <QTableView>

#include "droptarget.h"
#include "tablemodel.h"

class TableView : public QTableView, public DropTarget
{
	Q_OBJECT
public:
	explicit TableView(QWidget *parent = 0);

	virtual void setModel(QAbstractItemModel *model) override;

	void setDoubleClickTarget(DropTarget *target);
	virtual void notifyDragWasDropped() override;

protected:
	void focusInEvent(QFocusEvent *event) override;
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;
	void resizeEvent(QResizeEvent *event) override;

	virtual void dropEvent(QDropEvent *event) override;

private slots:
	void doubleClickedHandler(const QModelIndex index);

private:
	DropTarget *_defaultDropTarget;
	TableModel *_tableModel;
};

#endif // TABLEVIEWWIDGET_H
