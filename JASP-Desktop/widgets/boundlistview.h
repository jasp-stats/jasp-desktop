#ifndef BOUNDLISTVIEW_H
#define BOUNDLISTVIEW_H

#include "bound.h"

#include <QStringListModel>
#include <QListView>
#include <QIcon>

#include "options/optionvariables.h"

#include "availablefieldslistview.h"
#include "dataset.h"
#include "tablemodelvariablesassigned.h"
#include "listview.h"

class BoundListView : public ListView, public Bound
{
	Q_OBJECT

public:
	BoundListView(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;

	void setAssignButton(AssignButton *button);
	void setAvailableFieldsListView(AvailableFieldsListView *listView);

	virtual void setModel(QAbstractItemModel *model) OVERRIDE;

protected:
	virtual void resizeEvent(QResizeEvent *e) OVERRIDE;
	virtual void moveEvent(QMoveEvent *e) OVERRIDE;

	TableModelVariablesAssigned *_variablesListModel;

private:

	QWidget *_variableTypeKey;

private slots:
	void repositionKey();

};

#endif // BOUNDLISTVIEW_H
