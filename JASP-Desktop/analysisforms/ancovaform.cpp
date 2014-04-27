#include "ancovaform.h"
#include "ui_ancovaform.h"

#include "column.h"
#include "widgets/listmodelvariablesassigned.h"
#include "widgets/listmodelanovamodelnuisancefactors.h"

AncovaForm::AncovaForm(QWidget *parent) :
	AnalysisForm("AncovaForm", parent),
	ui(new Ui::AncovaForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);

	_dependentListModel = new ListModelVariablesAssigned(this);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	_dependentListModel->setSource(&_availableFields);
	ui->dependent->setModel(_dependentListModel);

	_fixedFactorsListModel = new ListModelVariablesAssigned(this);
	_fixedFactorsListModel->setSource(&_availableFields);
	_fixedFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->fixedFactors->setModel(_fixedFactorsListModel);

	_randomFactorsListModel = new ListModelVariablesAssigned(this);
	_randomFactorsListModel->setSource(&_availableFields);
	_randomFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->randomFactors->setModel(_randomFactorsListModel);

	_covariatesListModel = new ListModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableFields);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	ui->covariates->setModel(_covariatesListModel);

	_wlsWeightsListModel = new ListModelVariablesAssigned(this);
	_wlsWeightsListModel->setSource(&_availableFields);
	_wlsWeightsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_wlsWeightsListModel->setSource(&_availableFields);
	ui->wlsWeights->setModel(_wlsWeightsListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->randomFactors);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);
	ui->buttonAssignWLSWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);

	connect(_dependentListModel, SIGNAL(assignmentsChanged()), this, SLOT(dependentChanged()));
	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_randomFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new ListModelAnovaModel(this);
	ui->model->setModel(_anovaModel);
	ui->model->hide();
}

AncovaForm::~AncovaForm()
{
	delete ui;
}

void AncovaForm::factorsChanged()
{
	QList<ColumnInfo> factorsAvailable;

	factorsAvailable.append(_fixedFactorsListModel->assigned());
	factorsAvailable.append(_randomFactorsListModel->assigned());
	factorsAvailable.append(_covariatesListModel->assigned());

	_anovaModel->setVariables(factorsAvailable);
}

void AncovaForm::dependentChanged()
{
	const QList<ColumnInfo> &assigned = _dependentListModel->assigned();
	if (assigned.length() == 0)
		_anovaModel->setDependent(ColumnInfo("", 0));
	else
		_anovaModel->setDependent(assigned.last());
}
