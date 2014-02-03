#include "correlationform.h"
#include "ui_correlationform.h"

CorrelationForm::CorrelationForm(QWidget *parent) :
	AnalysisForm("CorrelationForm", parent),
	ui(new Ui::CorrelationForm)
{
	ui->setupUi(this);

	ui->availableVariables->setModel(&_availableFields);
	ui->availableVariables->setDoubleClickTarget(ui->variables);

	_modelVariables = new ListModelVariablesAssigned();
	_modelVariables->setSource(&_availableFields);
	ui->variables->setModel(_modelVariables);

	ui->assignButton->setSourceAndTarget(ui->availableVariables, ui->variables);

	ui->panelOptions->hide();
}

CorrelationForm::~CorrelationForm()
{
	delete ui;
}
