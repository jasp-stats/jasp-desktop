#include "ttestonesampleform.h"
#include "ui_ttestonesampleform.h"

#include "analysisform.h"
#include "widgets/tablemodelvariablesassigned.h"

TTestOneSampleForm::TTestOneSampleForm(QWidget *parent) :
	AnalysisForm("TTestOneSampleForm", parent),
	ui(new Ui::TTestOneSampleForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *variablesModel = new TableModelVariablesAssigned(this);
	variablesModel->setSource(&_availableVariablesModel);
	variablesModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	variablesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->variables);

	ui->confidenceIntervalInterval->setLabel("Confidence interval");
	ui->descriptivesPlotsConfidenceInterval->setLabel("Confidence interval");
}

TTestOneSampleForm::~TTestOneSampleForm()
{
	delete ui;
}
