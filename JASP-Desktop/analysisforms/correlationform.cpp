#include "correlationform.h"
#include "ui_correlationform.h"

CorrelationForm::CorrelationForm(QWidget *parent) :
	AnalysisForm("CorrelationForm", parent),
	ui(new Ui::CorrelationForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	_availableVariablesModel.setIsNominalTextAllowed(false);
	ui->availableVariables->setModel(&_availableVariablesModel);
	ui->availableVariables->setDoubleClickTarget(ui->variables);

	_modelVariables = new TableModelVariablesAssigned();
	_modelVariables->setSource(&_availableVariablesModel);
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
