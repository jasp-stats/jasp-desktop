#include "ttestindependentsamplesform.h"
#include "ui_ttestindependentsamplesform.h"

#include "analysisform.h"

TTestIndependentSamplesForm::TTestIndependentSamplesForm(QWidget *parent) :
	AnalysisForm("TTestIndependentSamplesForm", parent),
	ui(new Ui::TTestIndependentSamplesForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *variablesModel = new TableModelVariablesAssigned(this);
	variablesModel->setSource(&_availableVariablesModel);
	variablesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	variablesModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	TableModelVariablesAssigned *groupingVariableModel = new TableModelVariablesAssigned(this);
	groupingVariableModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	groupingVariableModel->setSource(&_availableVariablesModel);
	ui->groupingVariable->setModel(groupingVariableModel);
	ui->groupingVariable->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignGroupingVariable->setSourceAndTarget(ui->listAvailableFields, ui->groupingVariable);

	ui->confidenceIntervalInterval->setLabel("Confidence interval");
	ui->descriptivesPlotsConfidenceInterval->setLabel("Confidence interval");
}

TTestIndependentSamplesForm::~TTestIndependentSamplesForm()
{
	delete ui;
}
