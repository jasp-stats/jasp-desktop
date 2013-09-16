#ifndef LISTVIEWWITHFOCUSSIGNAL_H
#define LISTVIEWWITHFOCUSSIGNAL_H

#include <QListView>
#include <QAbstractItemView>

class ListView : public QListView
{
	Q_OBJECT
public:
	explicit ListView(QWidget *parent = 0);

	void setDoubleClickTarget(QAbstractItemView *target);
	
protected:
	void focusInEvent(QFocusEvent *event) override;
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;

signals:
	void selectionUpdated();
	void focused();

private slots:
	void doubleClickedHandler(const QModelIndex index);

private:
	QAbstractItemView *_defaultDropTarget;
	
};

#endif // LISTVIEWWITHFOCUSSIGNAL_H
