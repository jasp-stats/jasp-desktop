#include "ancovamultivariateform.h"
#include "ui_ancovamultivariateform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

AncovaMultivariateForm::AncovaMultivariateForm(QWidget *parent) :
	AnalysisForm("AncovaMultivariateForm", parent),
	ui(new Ui::AncovaMultivariateForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setSource(&_availableVariablesModel);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->dependents->setModel(_dependentListModel);

	_fixedFactorsListModel = new TableModelVariablesAssigned(this);
	_fixedFactorsListModel->setSource(&_availableVariablesModel);
	_fixedFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->fixedFactors->setModel(_fixedFactorsListModel);

	_covariatesListModel = new TableModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableVariablesModel);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->covariates->setModel(_covariatesListModel);

	_wlsWeightsListModel = new TableModelVariablesAssigned(this);
	_wlsWeightsListModel->setSource(&_availableVariablesModel);
	_wlsWeightsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_wlsWeightsListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_wlsWeightsListModel->setSource(&_availableVariablesModel);
	ui->wlsWeights->setModel(_wlsWeightsListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependents);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);
	ui->buttonAssignWLSWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);

	connect(_dependentListModel, SIGNAL(assignmentsChanged()), this, SLOT(dependentChanged()));
	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new TableModelAnovaModel(this);
	//ui->model->setModel(_anovaModel);
	ui->model->hide();
}

AncovaMultivariateForm::~AncovaMultivariateForm()
{
	delete ui;
}

void AncovaMultivariateForm::factorsChanged()
{
	Terms factorsAvailable;

	factorsAvailable.add(_fixedFactorsListModel->assigned());
	factorsAvailable.add(_covariatesListModel->assigned());

	_anovaModel->setVariables(factorsAvailable);
}

void AncovaMultivariateForm::dependentChanged()
{
	/*const QList<ColumnInfo> &assigned = _dependentListModel->assigned();
	if (assigned.length() == 0)
		_anovaModel->setDependent(ColumnInfo("", 0));
	else
		_anovaModel->setDependent(assigned.last());*/
}
