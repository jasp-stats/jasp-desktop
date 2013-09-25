#include "anovamultivariateform.h"
#include "ui_anovamultivariateform.h"

#include "column.h"
#include "widgets/listmodelvariablesassigned.h"
#include "widgets/listmodelanovamodelnuisancefactors.h"

AnovaMultivariateForm::AnovaMultivariateForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::AnovaMultivariateForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);

	_dependentListModel = new ListModelVariablesAssigned(this);
	_dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	_dependentListModel->setSource(&_availableFields);
	ui->dependents->setModel(_dependentListModel);

	_fixedFactorsListModel = new ListModelVariablesAssigned(this);
	_fixedFactorsListModel->setSource(&_availableFields);
	_fixedFactorsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->fixedFactors->setModel(_fixedFactorsListModel);

	_wlsWeightsListModel = new ListModelVariablesAssigned(this);
	_wlsWeightsListModel->setSource(&_availableFields);
	_wlsWeightsListModel->setVariableTypesAllowed(Column::ColumnTypeScale);
	_wlsWeightsListModel->setSource(&_availableFields);
	ui->wlsWeights->setModel(_wlsWeightsListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependents);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignWLSWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);

	connect(_dependentListModel, SIGNAL(assignmentsChanged()), this, SLOT(dependentChanged()));
	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new ListModelAnovaModel(this);
	ui->model->setModel(_anovaModel);
	ui->model->hide();
}

AnovaMultivariateForm::~AnovaMultivariateForm()
{
	delete ui;
}

void AnovaMultivariateForm::factorsChanged()
{
	_anovaModel->setVariables(_fixedFactorsListModel->assigned());
}

void AnovaMultivariateForm::dependentChanged()
{
	const QList<ColumnInfo> &assigned = _dependentListModel->assigned();
	if (assigned.length() == 0)
		_anovaModel->setDependent(ColumnInfo("", 0));
	else
		_anovaModel->setDependent(assigned.last());
}
