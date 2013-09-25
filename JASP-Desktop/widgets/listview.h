#ifndef LISTVIEWWITHFOCUSSIGNAL_H
#define LISTVIEWWITHFOCUSSIGNAL_H

#include <QListView>
#include <QAbstractItemView>

#include "droptarget.h"

class ListView : public QListView, public DropTarget
{
	Q_OBJECT
public:
	explicit ListView(QWidget *parent = 0);

	void setDoubleClickTarget(DropTarget *target);
	
protected:
	void focusInEvent(QFocusEvent *event) override;
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;

private slots:
	void doubleClickedHandler(const QModelIndex index);

private:
	DropTarget *_defaultDropTarget;
	
};

#endif // LISTVIEWWITHFOCUSSIGNAL_H
