#include "correlationpartialform.h"
#include "ui_correlationpartialform.h"

CorrelationPartialForm::CorrelationPartialForm(QWidget *parent) :
	AnalysisForm("CorrelationPartialForm", parent),
	ui(new Ui::CorrelationPartialForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	_modelVariables = new ListModelVariablesAssigned();
	_modelVariables->setSource(&_availableFields);
	ui->variables->setModel(_modelVariables);

	_modelControllingFor = new ListModelVariablesAssigned();
	_modelControllingFor->setSource(&_availableFields);
	ui->controllingFor->setModel(_modelControllingFor);

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignControllingFor->setSourceAndTarget(ui->listAvailableFields, ui->controllingFor);

	ui->panelOptions->hide();
}

CorrelationPartialForm::~CorrelationPartialForm()
{
	delete ui;
}
