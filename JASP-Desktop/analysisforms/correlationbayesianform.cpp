#include "correlationbayesianform.h"
#include "ui_correlationbayesianform.h"

CorrelationBayesianForm::CorrelationBayesianForm(QWidget *parent) :
	AnalysisForm("CorrelationBayesianForm", parent),
	ui(new Ui::CorrelationBayesianForm)
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
    ui->credibleIntervalsIntervalContainer->hide();
    ui->credibleIntervals->hide();
	ui->groupPrior->hide();
#else
	ui->optionsWidget->setStyleSheet("background-color: pink ;");
	ui->credibleIntervalsIntervalContainer->setStyleSheet("background-color: pink ;");
	ui->credibleIntervals->setStyleSheet("background-color: pink ;");
	ui->groupPrior->setStyleSheet("background-color: pink ;");
#endif

	ui->credibleIntervalsInterval->setLabel("Credible intervals");
}

CorrelationBayesianForm::~CorrelationBayesianForm()
{
	delete ui;
}
