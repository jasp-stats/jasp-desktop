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
