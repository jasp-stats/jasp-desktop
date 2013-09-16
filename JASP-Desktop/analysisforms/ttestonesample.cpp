#include "ttestonesample.h"
#include "ui_ttestonesample.h"

#include "analysisform.h"
#include "widgets/listmodelvariablesassigned.h"

TTestOneSample::TTestOneSample(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestOneSample)
{
	ui->setupUi(this);

	_availableFields.setVariableTypesAllowed(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	ListModelVariablesAssigned *variablesModel = new ListModelVariablesAssigned(this);
	variablesModel->setVariableTypesAllowed(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->variables);
}

TTestOneSample::~TTestOneSample()
{
	delete ui;
}
