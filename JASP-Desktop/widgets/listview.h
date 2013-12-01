#ifndef LISTVIEWWITHFOCUSSIGNAL_H
#define LISTVIEWWITHFOCUSSIGNAL_H

#include <QListView>
#include <QAbstractItemView>

#include "droptarget.h"
#include "listmodelvariables.h"

class ListView : public QListView, public DropTarget
{
	Q_OBJECT
public:
	explicit ListView(QWidget *parent = 0);

	void setDoubleClickTarget(DropTarget *target);
	virtual void setModel(QAbstractItemModel *model) override;
	virtual void notifyDragWasDropped() override;
	
protected:
	void focusInEvent(QFocusEvent *event) override;
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;
	void dropEvent(QDropEvent *event) override;

private slots:
	void doubleClickedHandler(const QModelIndex index);

private:
	DropTarget *_defaultDropTarget;
	ListModelVariables *_listModel;
	
};

#endif // LISTVIEWWITHFOCUSSIGNAL_H
