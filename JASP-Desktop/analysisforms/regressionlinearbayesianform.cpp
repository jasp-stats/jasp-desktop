#include "regressionlinearbayesianform.h"
#include "ui_regressionlinearbayesianform.h"

RegressionLinearBayesianForm::RegressionLinearBayesianForm(QWidget *parent) :
	AnalysisForm("RegressionLinearBayesianForm", parent),
	ui(new Ui::RegressionLinearBayesianForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_dependentListModel->setSource(&_availableVariablesModel);
	ui->dependent->setModel(_dependentListModel);

	_covariatesListModel = new TableModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableVariablesModel);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->covariates->setModel(_covariatesListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);

	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	ui->modelTerms->hide();

#ifdef QT_NO_DEBUG
	// temporary hides until the appropriate R code is implemented

	ui->posteriorDistributions->hide();
	ui->posteriorEstimates->hide();
#else
	ui->posteriorDistributions->setStyleSheet("background-color: pink;");
	ui->posteriorEstimates->setStyleSheet("background-color: pink;");
#endif
}

RegressionLinearBayesianForm::~RegressionLinearBayesianForm()
{
	delete ui;
}

void RegressionLinearBayesianForm::factorsChanged()
{
	_anovaModel->setVariables(_covariatesListModel->assigned());
}
