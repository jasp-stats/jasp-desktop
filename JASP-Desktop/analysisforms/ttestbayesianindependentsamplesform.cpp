#include "ttestbayesianindependentsamplesform.h"
#include "ui_ttestbayesianindependentsamplesform.h"

#include "analysisform.h"

TTestBayesianIndependentSamplesForm::TTestBayesianIndependentSamplesForm(QWidget *parent) :
	AnalysisForm("TTestBayesianIndependentSamplesForm", parent),
	ui(new Ui::TTestBayesianIndependentSamplesForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	ListModelVariablesAssigned *variablesModel = new ListModelVariablesAssigned(this);
	variablesModel->setSource(&_availableFields);
	variablesModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ListModelVariablesAssigned *groupingVariableModel = new ListModelVariablesAssigned(this);
	groupingVariableModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	groupingVariableModel->setSource(&_availableFields);
	ui->groupingVariable->setModel(groupingVariableModel);
	ui->groupingVariable->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignGroupingVariable->setSourceAndTarget(ui->listAvailableFields, ui->groupingVariable);
}

TTestBayesianIndependentSamplesForm::~TTestBayesianIndependentSamplesForm()
{
	delete ui;
}
