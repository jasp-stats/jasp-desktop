#include "regressionloglinearbayesianform.h"
#include "ui_regressionloglinearbayesianform.h"

RegressionLogLinearBayesianForm::RegressionLogLinearBayesianForm(QWidget *parent) :
	AnalysisForm("RegressionLogLinearBayesianForm", parent),
	ui(new Ui::RegressionLogLinearBayesianForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_dependentListModel->setSource(&_availableVariablesModel);
	ui->counts->setModel(_dependentListModel);

	_covariatesListModel = new TableModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableVariablesModel);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->factors->setModel(_covariatesListModel);

	ui->buttonAssignCounts->setSourceAndTarget(ui->listAvailableFields, ui->counts);
	ui->buttonAssignFactors->setSourceAndTarget(ui->listAvailableFields, ui->factors);

	_model = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_model);
	ui->modelTerms->hide();

	connect(_covariatesListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_covariatesListModel, SIGNAL(assignedTo(Terms)), _model, SLOT(addCovariates(Terms)));
	connect(_covariatesListModel, SIGNAL(unassigned(Terms)), _model, SLOT(removeVariables(Terms)));
}

RegressionLogLinearBayesianForm::~RegressionLogLinearBayesianForm()
{
	delete ui;
}

void RegressionLogLinearBayesianForm:: bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	_model->setVariables(Terms(), Terms(), _covariatesListModel->assigned());

	factorsChanged();
}

void RegressionLogLinearBayesianForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void RegressionLogLinearBayesianForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}
