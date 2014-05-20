#include "ttestbayesianonesampleform.h"
#include "ui_ttestbayesianonesampleform.h"

TTestBayesianOneSampleForm::TTestBayesianOneSampleForm(QWidget *parent) :
	AnalysisForm("TTestBayesianOneSampleForm", parent),
	ui(new Ui::TTestBayesianOneSampleForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *variablesModel = new TableModelVariablesAssigned(this);
	variablesModel->setSource(&_availableVariablesModel);
	variablesModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->variables);
}

TTestBayesianOneSampleForm::~TTestBayesianOneSampleForm()
{
	delete ui;
}
