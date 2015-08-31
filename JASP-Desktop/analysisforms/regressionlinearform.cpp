#include "regressionlinearform.h"
#include "ui_regressionlinearform.h"

RegressionLinearForm::RegressionLinearForm(QWidget *parent) :
	AnalysisForm("RegressionLinearForm", parent),
	ui(new Ui::RegressionLinearForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->listAvailableFields->setModel(&this->_availableVariablesModel);

	_dependentModel = new TableModelVariablesAssigned();
	_dependentModel->setSource(&_availableVariablesModel);
	_dependentModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->dependent->setModel(_dependentModel);

	ui->method->addItem("Enter");
	ui->method->addItem("Backward");
	ui->method->addItem("Forward");
	ui->method->addItem("Stepwise");

	_covariatesModel = new TableModelVariablesAssigned();
	_covariatesModel->setSource(&_availableVariablesModel);
	_covariatesModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	_covariatesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	ui->covariates->setModel(_covariatesModel);

	_wlsWeightsModel = new TableModelVariablesAssigned();
	_wlsWeightsModel->setSource(&_availableVariablesModel);
	_wlsWeightsModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_wlsWeightsModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->wlsWeights->setModel(_wlsWeightsModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignBlocks->setSourceAndTarget(ui->listAvailableFields, ui->covariates);
	ui->buttonAssignWlsWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);

	_modelModel = new TableModelAnovaModel(this);
	_modelModel->setPiecesCanBeAssigned(false);
	ui->modelTerms->setModel(_modelModel);
	ui->modelTerms->hide();

	connect(_covariatesModel, SIGNAL(assignedTo(Terms)), _modelModel, SLOT(addCovariates(Terms)));
	connect(_covariatesModel, SIGNAL(unassigned(Terms)), _modelModel, SLOT(removeVariables(Terms)));

	ui->panelStatistics->hide();
	ui->panelOptions->hide();
    ui->panelAssumptionChecks->hide();

#ifdef QT_NO_DEBUG
    ui->missingValues->hide();
#else
    ui->missingValues->setStyleSheet("background-color: pink ;");
#endif

}

RegressionLinearForm::~RegressionLinearForm()
{
	delete ui;
}

void RegressionLinearForm:: bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);
	_modelModel->setVariables(Terms(), Terms(), _covariatesModel->assigned());
}
