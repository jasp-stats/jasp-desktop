#include "ttestindependentsamplesform.h"
#include "ui_ttestindependentsamplesform.h"

#include "analysisform.h"

TTestIndependentSamplesForm::TTestIndependentSamplesForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestIndependentSamplesForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	ListModelVariablesAssigned *variablesModel = new ListModelVariablesAssigned(this);
	variablesModel->setSource(&_availableFields);
	variablesModel->setVariableTypesAllowed(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ListModelVariablesAssigned *groupingVariableModel = new ListModelVariablesAssigned(this);
	groupingVariableModel->setVariableTypesAllowed(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
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
