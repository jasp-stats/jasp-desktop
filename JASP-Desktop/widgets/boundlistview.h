#ifndef BOUNDLISTVIEW_H
#define BOUNDLISTVIEW_H

#include "bound.h"

#include <QStringListModel>
#include <QListView>

#include "options/optionfields.h"

#include "availablefieldslistview.h"
#include "assignbutton.h"

class BoundListView : public QListView, public Bound
{
	Q_OBJECT

public:
	BoundListView(QWidget *parent = 0);

	virtual void bindTo(Option *option) override;

	void setAssignButton(AssignButton *button);
	void setAvailableFieldsListView(AvailableFieldsListView *listView);

protected:
	virtual void focusInEvent(QFocusEvent *event) override;
	virtual void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;

private slots:
	void assign();

private:
	void updateList();
	QStringListModel _listModel;
	OptionFields *_boundTo;

	AvailableFieldsListView *_availableFieldsListView;
	AssignButton *_assignButton;
};

#endif // BOUNDLISTVIEW_H
