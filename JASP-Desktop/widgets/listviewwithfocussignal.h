#ifndef LISTVIEWWITHFOCUSSIGNAL_H
#define LISTVIEWWITHFOCUSSIGNAL_H

#include <QListView>

class ListViewWithFocusSignal : public QListView
{
	Q_OBJECT
public:
	explicit ListViewWithFocusSignal(QWidget *parent = 0);
	
protected:
	void focusInEvent(QFocusEvent *event) override;
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;

signals:
	void focused();
	
public slots:
	
};

#endif // LISTVIEWWITHFOCUSSIGNAL_H
