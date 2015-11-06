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

	_factorsListModel = new TableModelVariablesAssigned(this);
	_factorsListModel->setSource(&_availableVariablesModel);
	_factorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->factors->setModel(_factorsListModel);

	ui->buttonAssignCounts->setSourceAndTarget(ui->listAvailableFields, ui->counts);
	ui->buttonAssignFactors->setSourceAndTarget(ui->listAvailableFields, ui->factors);

	_model = new TableModelAnovaModel(this);
	_model->setPiecesCanBeAssigned(false);
	ui->modelTerms->setModel(_model);
	ui->modelTerms->hide();

	connect(_factorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_factorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_factorsListModel, SIGNAL(assignedTo(Terms)), _model, SLOT(addCovariates(Terms)));
	connect(_factorsListModel, SIGNAL(unassigned(Terms)), _model, SLOT(removeVariables(Terms)));

	ui->posteriorProbabilityCutOff->setLabel("Posterior prob. cut-off");
	ui->maxModels->setLabel("Display best N models");

	ui->panelStatistics->hide();
	ui->advancedWidget->hide();
}

RegressionLogLinearBayesianForm::~RegressionLogLinearBayesianForm()
{
	delete ui;
}

void RegressionLogLinearBayesianForm:: bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	_model->setVariables(Terms(), Terms(), _factorsListModel->assigned());

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
