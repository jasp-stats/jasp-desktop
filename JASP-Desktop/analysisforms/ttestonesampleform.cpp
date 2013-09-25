#include "ttestonesampleform.h"
#include "ui_ttestonesampleform.h"

#include "analysisform.h"
#include "widgets/listmodelvariablesassigned.h"

TTestOneSampleForm::TTestOneSampleForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestOneSampleForm)
{
	ui->setupUi(this);

	_availableFields.setVariableTypesAllowed(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	ListModelVariablesAssigned *variablesModel = new ListModelVariablesAssigned(this);
	variablesModel->setSource(&_availableFields);
	variablesModel->setVariableTypesAllowed(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->variables);
}

TTestOneSampleForm::~TTestOneSampleForm()
{
	delete ui;
}
