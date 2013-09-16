#ifndef BOUNDLISTVIEW_H
#define BOUNDLISTVIEW_H

#include "bound.h"

#include <QStringListModel>
#include <QListView>
#include <QIcon>

#include "options/optionfields.h"

#include "availablefieldslistview.h"
#include "assignbutton.h"
#include "dataset.h"
#include "listmodelvariablesassigned.h"
#include "listview.h"

class BoundListView : public ListView, public Bound
{
	Q_OBJECT

public:
	BoundListView(QWidget *parent = 0);

	virtual void bindTo(Option *option) override;

	void setAssignButton(AssignButton *button);
	void setAvailableFieldsListView(AvailableFieldsListView *listView);

	virtual void setModel(QAbstractItemModel *model) override;

protected:
	virtual void resizeEvent(QResizeEvent *e) override;
	virtual void moveEvent(QMoveEvent *e) override;

	ListModelVariablesAssigned *_variablesListModel;

private:

	AvailableFieldsListView *_availableFieldsListView;
	AssignButton *_assignButton;

	QWidget *_variableTypeKey;

private slots:
	void repositionKey();

};

#endif // BOUNDLISTVIEW_H
