#include "correlationform.h"
#include "ui_correlationform.h"

CorrelationForm::CorrelationForm(QWidget *parent) :
	AnalysisForm("CorrelationForm", parent),
	ui(new Ui::CorrelationForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->availableVariables->setModel(&_availableVariablesModel);
	ui->availableVariables->setDoubleClickTarget(ui->variables);

	_modelVariables = new TableModelVariablesAssigned();
	_modelVariables->setSource(&_availableVariablesModel);
	_modelVariables->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	_modelVariables->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	ui->variables->setModel(_modelVariables);

	ui->assignButton->setSourceAndTarget(ui->availableVariables, ui->variables);

	ui->panelOptions->hide();

#ifdef QT_NO_DEBUG
	ui->optionsWidget->hide();
#else
	ui->optionsWidget->setStyleSheet("background-color: pink ;");
#endif


}

CorrelationForm::~CorrelationForm()
{
	delete ui;
}
