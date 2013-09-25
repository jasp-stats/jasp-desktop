#ifndef TABLEVIEWWIDGET_H
#define TABLEVIEWWIDGET_H

#include <QTableView>

#include "droptarget.h"

class TableView : public QTableView, public DropTarget
{
	Q_OBJECT
public:
	explicit TableView(QWidget *parent = 0);

	void setDoubleClickTarget(DropTarget *target);

protected:
	void focusInEvent(QFocusEvent *event) override;
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;
	void resizeEvent(QResizeEvent *event) override;

private slots:
	void doubleClickedHandler(const QModelIndex index);

private:
	DropTarget *_defaultDropTarget;
};

#endif // TABLEVIEWWIDGET_H
