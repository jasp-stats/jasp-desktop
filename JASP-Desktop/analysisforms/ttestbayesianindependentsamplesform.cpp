#include "ttestbayesianindependentsamplesform.h"
#include "ui_ttestbayesianindependentsamplesform.h"

#include "analysisform.h"

TTestBayesianIndependentSamplesForm::TTestBayesianIndependentSamplesForm(QWidget *parent) :
	AnalysisForm("TTestBayesianIndependentSamplesForm", parent),
	ui(new Ui::TTestBayesianIndependentSamplesForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *variablesModel = new TableModelVariablesAssigned(this);
	variablesModel->setSource(&_availableVariablesModel);
	variablesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	variablesModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	TableModelVariablesAssigned *groupingVariableModel = new TableModelVariablesAssigned(this);
	groupingVariableModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	groupingVariableModel->setSource(&_availableVariablesModel);
	ui->groupingVariable->setModel(groupingVariableModel);
	ui->groupingVariable->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignGroupingVariable->setSourceAndTarget(ui->listAvailableFields, ui->groupingVariable);

#ifdef QT_NO_DEBUG
	ui->plotBayesFactorRobustness->hide();
	ui->plotSequentialAnalysis->hide();
	ui->plotSequentialAnalysisRobustness->hide();
#else
	ui->plotBayesFactorRobustness->setStyleSheet("background-color: pink;");
	ui->plotSequentialAnalysis->setStyleSheet("background-color: pink;");
	ui->plotSequentialAnalysisRobustness->setStyleSheet("background-color: pink;");
#endif
}

TTestBayesianIndependentSamplesForm::~TTestBayesianIndependentSamplesForm()
{
	delete ui;
}
