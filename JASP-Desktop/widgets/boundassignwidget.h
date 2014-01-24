#ifndef BOUNDASSIGNWIDGET_H
#define BOUNDASSIGNWIDGET_H

#include <QWidget>

#include "bound.h"
#include "common.h"
#include "options/optionfields.h"
#include "widgets/listmodelvariablesavailable.h"
#include "widgets/listmodelvariablesassigned.h"

#include <QAbstractItemModel>

namespace Ui {
class BoundAssignWidget;
}

class BoundAssignWidget : public QWidget, public Bound
{
	Q_OBJECT

public:
	explicit BoundAssignWidget(QWidget *parent = 0);
	~BoundAssignWidget();

	virtual void bindTo(Option *option) OVERRIDE;

	void setVariables(const QList<ColumnInfo> &variables);
private:
	Ui::BoundAssignWidget *ui;

	ListModelVariablesAvailable *_availableModel;
	ListModelVariablesAssigned *_assignedModel;
};

#endif // BOUNDASSIGNWIDGET_H
