#ifndef BOUNDASSIGNWIDGET_H
#define BOUNDASSIGNWIDGET_H

#include <QWidget>

#include "bound.h"
#include "common.h"
#include "options/optionterms.h"
#include "widgets/tablemodelvariablesavailable.h"
#include "widgets/tablemodelvariablesassigned.h"

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

	void setVariables(const Terms &variables);
private:
	Ui::BoundAssignWidget *ui;

	TableModelVariablesAvailable *_availableModel;
	TableModelVariablesAssigned *_assignedModel;
};

#endif // BOUNDASSIGNWIDGET_H
