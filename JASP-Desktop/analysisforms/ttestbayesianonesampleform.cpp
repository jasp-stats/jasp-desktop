#include "ttestbayesianonesampleform.h"
#include "ui_ttestbayesianonesampleform.h"

TTestBayesianOneSampleForm::TTestBayesianOneSampleForm(QWidget *parent) :
	AnalysisForm("TTestBayesianOneSampleForm", parent),
	ui(new Ui::TTestBayesianOneSampleForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *variablesModel = new TableModelVariablesAssigned(this);
	variablesModel->setSource(&_availableVariablesModel);
	variablesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	variablesModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->variables);

#ifdef QT_NO_DEBUG
	ui->additionalStatisticsGroup->hide();
	ui->plotBayesFactorRobustness->hide();
	ui->plotSequentialAnalysis->hide();
	ui->plotSequentialAnalysisRobustness->hide();
#else
	ui->additionalStatisticsGroup->setStyleSheet("background-color: pink;");
	ui->plotBayesFactorRobustness->setStyleSheet("background-color: pink;");
	ui->plotSequentialAnalysis->setStyleSheet("background-color: pink;");
	ui->plotSequentialAnalysisRobustness->setStyleSheet("background-color: pink;");
#endif
}

TTestBayesianOneSampleForm::~TTestBayesianOneSampleForm()
{
	delete ui;
}
