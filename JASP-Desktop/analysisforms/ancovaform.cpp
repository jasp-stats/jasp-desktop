#include "ancovaform.h"
#include "ui_ancovaform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

AncovaForm::AncovaForm(QWidget *parent) :
	AnalysisForm("AncovaForm", parent),
	ui(new Ui::AncovaForm)
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

	_covariatesListModel = new TableModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableVariablesModel);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->covariates->setModel(_covariatesListModel);

	_wlsWeightsListModel = new TableModelVariablesAssigned(this);
	_wlsWeightsListModel->setSource(&_availableVariablesModel);
	_wlsWeightsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_wlsWeightsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->wlsWeights->setModel(_wlsWeightsListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->randomFactors);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);
	ui->buttonAssignWLSWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);

	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_randomFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	connect(_anovaModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	termsChanged();

	_contrastsModel = new TableModelVariablesOptions();
    ui->contrasts->setModel(_contrastsModel);

    _plotFactorsAvailableTableModel = new TableModelVariablesAvailable();
    _plotFactorsAvailableTableModel->setInfoProvider(this);
    ui->plot_variables->setModel(_plotFactorsAvailableTableModel);

    _horizontalAxisTableModel = new TableModelVariablesAssigned(this);
    _horizontalAxisTableModel->setSource(_plotFactorsAvailableTableModel);
    ui->horizontalAxis->setModel(_horizontalAxisTableModel);

    _seperateLinesTableModel = new TableModelVariablesAssigned(this);
    _seperateLinesTableModel->setSource(_plotFactorsAvailableTableModel);
    ui->seperateLines->setModel(_seperateLinesTableModel);

    _seperatePlotsTableModel = new TableModelVariablesAssigned(this);
    _seperatePlotsTableModel->setSource(_plotFactorsAvailableTableModel);
    ui->seperatePlots->setModel(_seperatePlotsTableModel);

    ui->buttonAssignHorizontalAxis->setSourceAndTarget(ui->plot_variables, ui->horizontalAxis);
    ui->buttonAssignSeperateLines->setSourceAndTarget(ui->plot_variables, ui->seperateLines);
    ui->buttonAssignSeperatePlots->setSourceAndTarget(ui->plot_variables, ui->seperatePlots);

	ui->containerModel->hide();
	ui->containerFactors->hide();
	ui->containerOptions->hide();
	ui->containerPostHocTests->hide();
    ui->containerProfilePlot->hide();

#ifdef QT_NO_DEBUG
	ui->groupComareMainEffects->hide();
	ui->marginalMeansContainer->hide();
    ui->profilePlot->hide();
    ui->misc_factorCovariateIndependence->hide();
#else
	ui->groupComareMainEffects->setStyleSheet("background-color: pink ;");
	ui->marginalMeansContainer->setStyleSheet("background-color: pink ;");
    ui->profilePlot->setStyleSheet("background-color: pink ;");
    ui->misc_factorCovariateIndependence->setStyleSheet("background-color: pink ;");
#endif

}

AncovaForm::~AncovaForm()
{
	delete ui;
}

void AncovaForm::factorsChanged()
{
	Terms factorsAvailable;

	factorsAvailable.add(_fixedFactorsListModel->assigned());
	factorsAvailable.add(_randomFactorsListModel->assigned());

	_anovaModel->setVariables(factorsAvailable, _covariatesListModel->assigned());
	_contrastsModel->setVariables(factorsAvailable);
    _plotFactorsAvailableTableModel->setVariables(factorsAvailable);

	ui->postHocTests_variables->setVariables(factorsAvailable);
}

void AncovaForm::termsChanged()
{
	Terms terms;

	terms.add(string("~OVERALL"));
	terms.add(_anovaModel->terms());

	ui->marginalMeans_terms->setVariables(terms);
}
