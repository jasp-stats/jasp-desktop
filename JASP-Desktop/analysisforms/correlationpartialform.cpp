#include "correlationpartialform.h"
#include "ui_correlationpartialform.h"

CorrelationPartialForm::CorrelationPartialForm(QWidget *parent) :
	AnalysisForm("CorrelationPartialForm", parent),
	ui(new Ui::CorrelationPartialForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	_modelVariables = new TableModelVariablesAssigned();
	_modelVariables->setSource(&_availableVariablesModel);
	ui->variables->setModel(_modelVariables);

	_modelControllingFor = new TableModelVariablesAssigned();
	_modelControllingFor->setSource(&_availableVariablesModel);
	ui->controllingFor->setModel(_modelControllingFor);

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignControllingFor->setSourceAndTarget(ui->listAvailableFields, ui->controllingFor);

	ui->panelOptions->hide();
}

CorrelationPartialForm::~CorrelationPartialForm()
{
	delete ui;
}
