#include "ttestindependentsamplesform.h"
#include "ui_ttestindependentsamplesform.h"

#include "analysisform.h"

TTestIndependentSamplesForm::TTestIndependentSamplesForm(QWidget *parent) :
	AnalysisForm("TTestIndependentSamplesForm", parent),
	ui(new Ui::TTestIndependentSamplesForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	ListModelVariablesAssigned *variablesModel = new ListModelVariablesAssigned(this);
	variablesModel->setSource(&_availableFields);
	variablesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	variablesModel->setIsNominalTextAllowed(false);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ListModelVariablesAssigned *groupingVariableModel = new ListModelVariablesAssigned(this);
	groupingVariableModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	groupingVariableModel->setIsNominalTextAllowed(true);
	groupingVariableModel->setSource(&_availableFields);
	ui->groupingVariable->setModel(groupingVariableModel);
	ui->groupingVariable->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignGroupingVariable->setSourceAndTarget(ui->listAvailableFields, ui->groupingVariable);
}

TTestIndependentSamplesForm::~TTestIndependentSamplesForm()
{
	delete ui;
}
