#include "anovaform.h"
#include "ui_anovaform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

AnovaForm::AnovaForm(QWidget *parent) :
	AnalysisForm("AnovaForm", parent),
	ui(new Ui::AnovaForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	_dependentListModel->setSource(&_availableVariablesModel);
	ui->dependent->setModel(_dependentListModel);

	_fixedFactorsListModel = new TableModelVariablesAssigned(this);
	_fixedFactorsListModel->setSource(&_availableVariablesModel);
	_fixedFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->fixedFactors->setModel(_fixedFactorsListModel);

	_randomFactorsListModel = new TableModelVariablesAssigned(this);
	_randomFactorsListModel->setSource(&_availableVariablesModel);
	_randomFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->randomFactors->setModel(_randomFactorsListModel);

	_wlsWeightsListModel = new TableModelVariablesAssigned(this);
	_wlsWeightsListModel->setSource(&_availableVariablesModel);
	_wlsWeightsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_wlsWeightsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->wlsWeights->setModel(_wlsWeightsListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->randomFactors);
	ui->buttonAssignWLSWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);

	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_randomFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	connect(_anovaModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	termsChanged();

	_contrastsModel = new TableModelVariablesOptions();
    ui->contrasts->setModel(_contrastsModel);

	ui->containerModel->hide();
	ui->containerFactors->hide();
	ui->containerOptions->hide();
	ui->containerPostHocTests->hide();


#ifdef QT_NO_DEBUG
	ui->groupComareMainEffects->hide();
	ui->groupOptions->hide();
	ui->groupComareMainEffects->hide();
	ui->groupPostHocOptions->hide();
	ui->groupEqualVariances->hide();
#else
	ui->groupComareMainEffects->setStyleSheet("background-color: pink ;");
	ui->groupOptions->setStyleSheet("background-color: pink ;");
	ui->groupComareMainEffects->setStyleSheet("background-color: pink ;");
	ui->groupPostHocOptions->setStyleSheet("background-color: pink ;");
	ui->groupEqualVariances->setStyleSheet("background-color: pink ;");
#endif

}

AnovaForm::~AnovaForm()
{
	delete ui;
}

void AnovaForm::factorsChanged()
{
	Terms factorsAvailable;

	factorsAvailable.add(_fixedFactorsListModel->assigned());
	factorsAvailable.add(_randomFactorsListModel->assigned());

	_anovaModel->setVariables(factorsAvailable);
	_contrastsModel->setVariables(factorsAvailable);

	ui->postHocTests_variables->setVariables(factorsAvailable);
}

void AnovaForm::termsChanged()
{
	Terms terms = _anovaModel->terms();
	terms.insert(0, string("~OVERALL"));
	ui->marginalMeans_terms->setVariables(terms);
}
