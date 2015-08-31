#include "regressionloglinearform.h"
#include "ui_regressionloglinearform.h"

RegressionLogLinearForm::RegressionLogLinearForm(QWidget *parent) :
	AnalysisForm("RegressionLogLinearForm", parent),
	ui(new Ui::RegressionLogLinearForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_countsModel = new TableModelVariablesAssigned();
	_countsModel->setSource(&_availableVariablesModel);
	_countsModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_countsModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->counts->setModel(_countsModel);

	ui->method->addItem("Enter");
	ui->method->addItem("Backward");
	ui->method->addItem("Forward");

	_factorsModel = new TableModelVariablesAssigned();
	_factorsModel->setSource(&_availableVariablesModel);
	_factorsModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->factors->setModel(_factorsModel);

	ui->buttonAssignCounts->setSourceAndTarget(ui->listAvailableFields, ui->counts);
	ui->buttonAssignFactors->setSourceAndTarget(ui->listAvailableFields, ui->factors);

	_model = new TableModelAnovaModel(this);
	_model->setPiecesCanBeAssigned(false);
	ui->modelTerms->setModel(_model);
	ui->modelTerms->hide();

	connect(_factorsModel, SIGNAL(assignedTo(Terms)), _model, SLOT(addCovariates(Terms)));
	connect(_factorsModel, SIGNAL(unassigned(Terms)), _model, SLOT(removeVariables(Terms)));

	ui->panelStatistics->hide();
}

RegressionLogLinearForm::~RegressionLogLinearForm()
{
	delete ui;
}

void RegressionLogLinearForm:: bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);
	_model->setVariables(Terms(), Terms(), _factorsModel->assigned());
}
