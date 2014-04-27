#include "correlationform.h"
#include "ui_correlationform.h"

CorrelationForm::CorrelationForm(QWidget *parent) :
	AnalysisForm("CorrelationForm", parent),
	ui(new Ui::CorrelationForm)
{
	ui->setupUi(this);

	_availableFields.setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	_availableFields.setIsNominalTextAllowed(false);
	ui->availableVariables->setModel(&_availableFields);
	ui->availableVariables->setDoubleClickTarget(ui->variables);

	_modelVariables = new ListModelVariablesAssigned();
	_modelVariables->setSource(&_availableFields);
	_modelVariables->setIsNominalTextAllowed(false);
	_modelVariables->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	ui->variables->setModel(_modelVariables);

	ui->assignButton->setSourceAndTarget(ui->availableVariables, ui->variables);

	ui->panelOptions->hide();
}

CorrelationForm::~CorrelationForm()
{
	delete ui;
}
