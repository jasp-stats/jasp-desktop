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

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	connect(_anovaModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_fixedFactorsListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addFixedFactors(Terms)));
	connect(_fixedFactorsListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	connect(_randomFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_randomFactorsListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addRandomFactors(Terms)));
	connect(_randomFactorsListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

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
#else
	ui->groupComareMainEffects->setStyleSheet("background-color: pink ;");
	ui->marginalMeansContainer->setStyleSheet("background-color: pink ;");
    ui->profilePlot->setStyleSheet("background-color: pink ;");
#endif

}

AnovaForm::~AnovaForm()
{
	delete ui;
}

void AnovaForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	_anovaModel->setVariables(_fixedFactorsListModel->assigned(), _randomFactorsListModel->assigned());

	factorsChanged();
	termsChanged();
}

void AnovaForm::factorsChanged()
{
	Terms factorsAvailable;

	factorsAvailable.add(_fixedFactorsListModel->assigned());
	factorsAvailable.add(_randomFactorsListModel->assigned());

	_contrastsModel->setVariables(factorsAvailable);
    _plotFactorsAvailableTableModel->setVariables(factorsAvailable);

	ui->postHocTests_variables->setVariables(factorsAvailable);
}

void AnovaForm::termsChanged()
{
	Terms terms;

	terms.add(string("~OVERALL"));
	terms.add(_anovaModel->terms());

	ui->marginalMeans_terms->setVariables(terms);
}
