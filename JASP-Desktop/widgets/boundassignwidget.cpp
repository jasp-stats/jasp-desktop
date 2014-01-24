#include "boundassignwidget.h"
#include "ui_boundassignwidget.h"

BoundAssignWidget::BoundAssignWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::BoundAssignWidget)
{
	ui->setupUi(this);

	_availableModel = new ListModelVariablesAvailable(this);
	_assignedModel = new ListModelVariablesAssigned(this);

	_assignedModel->setSource(_availableModel);

	ui->availableVariables->setModel(_availableModel);
	ui->availableVariables->setDoubleClickTarget(ui->assignedVariables);

	ui->assignedVariables->setModel(_assignedModel);

	ui->assignButton->setSourceAndTarget(ui->availableVariables, ui->assignedVariables);
}

BoundAssignWidget::~BoundAssignWidget()
{
	delete ui;
}

void BoundAssignWidget::bindTo(Option *option)
{
	ui->assignedVariables->bindTo(option);
}

void BoundAssignWidget::setVariables(const QList<ColumnInfo> &variables)
{
	_availableModel->setVariables(variables);
	_availableModel->notifyAlreadyAssigned(_assignedModel->assigned());
}
