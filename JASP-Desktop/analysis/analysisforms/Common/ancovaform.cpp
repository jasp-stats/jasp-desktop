//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

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
    _fixedFactorsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeNominalText | Column::ColumnTypeOrdinal);
    ui->fixedFactors->setModel(_fixedFactorsListModel);

	_randomFactorsListModel = new TableModelVariablesAssigned(this);
	_randomFactorsListModel->setSource(&_availableVariablesModel);
	_randomFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
    _randomFactorsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeNominalText | Column::ColumnTypeOrdinal);
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

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	connect(_anovaModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_fixedFactorsListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addFixedFactors(Terms)));
	connect(_fixedFactorsListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	connect(_randomFactorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_randomFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_randomFactorsListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addRandomFactors(Terms)));
	connect(_randomFactorsListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	connect(_covariatesListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_covariatesListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addCovariates(Terms)));
	connect(_covariatesListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	_contrastsModel = new TableModelVariablesOptions();
    ui->contrasts->setModel(_contrastsModel);

    _plotFactorsAvailableTableModel = new TableModelVariablesAvailable();
    _plotFactorsAvailableTableModel->setInfoProvider(this);
	ui->plotVariables->setModel(_plotFactorsAvailableTableModel);

    _horizontalAxisTableModel = new TableModelVariablesAssigned(this);
    _horizontalAxisTableModel->setSource(_plotFactorsAvailableTableModel);
	ui->plotHorizontalAxis->setModel(_horizontalAxisTableModel);

    _seperateLinesTableModel = new TableModelVariablesAssigned(this);
    _seperateLinesTableModel->setSource(_plotFactorsAvailableTableModel);
	ui->plotSeparateLines->setModel(_seperateLinesTableModel);

    _seperatePlotsTableModel = new TableModelVariablesAssigned(this);
    _seperatePlotsTableModel->setSource(_plotFactorsAvailableTableModel);
	ui->plotSeparatePlots->setModel(_seperatePlotsTableModel);

	ui->buttonAssignHorizontalAxis->setSourceAndTarget(ui->plotVariables, ui->plotHorizontalAxis);
	ui->buttonAssignSeperateLines->setSourceAndTarget(ui->plotVariables, ui->plotSeparateLines);
	ui->buttonAssignSeperatePlots->setSourceAndTarget(ui->plotVariables, ui->plotSeparatePlots);

    _simpleEffectsAvailableTableModel = new TableModelVariablesAvailable();
    _simpleEffectsAvailableTableModel->setInfoProvider(this);
	ui->simpleEffectsVariables->setModel(_simpleEffectsAvailableTableModel);

    _simpleFactorTableModel = new TableModelVariablesAssigned(this);
    _simpleFactorTableModel->setSource(_simpleEffectsAvailableTableModel);
	ui->simpleFactor->setModel(_simpleFactorTableModel);

    _moderatorOneTableModel = new TableModelVariablesAssigned(this);
    _moderatorOneTableModel->setSource(_simpleEffectsAvailableTableModel);
	ui->moderatorFactorOne->setModel(_moderatorOneTableModel);

    _moderatorTwoTableModel = new TableModelVariablesAssigned(this);
    _moderatorTwoTableModel->setSource(_simpleEffectsAvailableTableModel);
	ui->moderatorFactorTwo->setModel(_moderatorTwoTableModel);
	
	ui->buttonAssignSimpleFactor->setSourceAndTarget(ui->simpleEffectsVariables, ui->simpleFactor);
	ui->buttonAssignModeratorOne->setSourceAndTarget(ui->simpleEffectsVariables, ui->moderatorFactorOne);
	ui->buttonAssignModeratorTwo->setSourceAndTarget(ui->simpleEffectsVariables, ui->moderatorFactorTwo);

		_kruskalAvailableTableModel = new TableModelVariablesAvailable();
		_kruskalAvailableTableModel->setInfoProvider(this);
	ui->kruskalVariables->setModel(_kruskalAvailableTableModel);

		_kruskalTableModel = new TableModelVariablesAssigned(this);
		_kruskalTableModel->setSource(_kruskalAvailableTableModel);
	ui->kruskalVariablesAssigned->setModel(_kruskalTableModel);

	ui->buttonAssignKruskal->setSourceAndTarget(ui->kruskalVariables, ui->kruskalVariablesAssigned);

	ui->containerModel->hide();
	ui->containerFactors->hide();
	ui->containerOptions->hide();
	ui->containerSimpleEffect->hide();
	ui->containerPostHocTests->hide();
	ui->containerDescriptivesPlot->hide();
	ui->containerAssumptions->hide();
	ui->containerKruskal->hide();

	ui->confidenceIntervalInterval->setLabel("Confidence interval");

#ifndef JASP_DEBUG
	ui->factorCovariateIndependence->hide();
	ui->randomFactors->hide();
	ui->buttonAssignRandom->hide();
	ui->label_3->hide();
#else
	ui->factorCovariateIndependence->setStyleSheet("background-color: pink ;");
	ui->randomFactors->setStyleSheet("background-color: pink ;");
	ui->buttonAssignRandom->setStyleSheet("background-color: pink ;");
	ui->label_3->setStyleSheet("background-color: pink ;");
#endif

}

AncovaForm::~AncovaForm()
{
	delete ui;
}

void AncovaForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	_anovaModel->setVariables(_fixedFactorsListModel->assigned(), _randomFactorsListModel->assigned(), _covariatesListModel->assigned());

	factorsChanged();
	termsChanged();
}

void AncovaForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void AncovaForm::factorsChanged()
{
	Terms factorsAvailable;

	factorsAvailable.add(_fixedFactorsListModel->assigned());
	factorsAvailable.add(_randomFactorsListModel->assigned());

	_contrastsModel->setVariables(factorsAvailable);
	_plotFactorsAvailableTableModel->setVariables(factorsAvailable);
	_simpleEffectsAvailableTableModel->setVariables(factorsAvailable);
	_kruskalAvailableTableModel->setVariables(factorsAvailable);

	Terms plotVariablesAssigned;
	plotVariablesAssigned.add(_horizontalAxisTableModel->assigned());
	plotVariablesAssigned.add(_seperateLinesTableModel->assigned());
	plotVariablesAssigned.add(_seperatePlotsTableModel->assigned());
	_plotFactorsAvailableTableModel->notifyAlreadyAssigned(plotVariablesAssigned);

	Terms simpleEffectsVariablesAssigned;
	simpleEffectsVariablesAssigned.add(_simpleFactorTableModel->assigned());
	simpleEffectsVariablesAssigned.add(_moderatorOneTableModel->assigned());
	simpleEffectsVariablesAssigned.add(_moderatorTwoTableModel->assigned());
	_simpleEffectsAvailableTableModel->notifyAlreadyAssigned(simpleEffectsVariablesAssigned);

	Terms kruskalVariablesAssigned;
	kruskalVariablesAssigned.add(_kruskalTableModel->assigned());
	_kruskalAvailableTableModel->notifyAlreadyAssigned(kruskalVariablesAssigned);
	
    	ui->postHocTestsVariables->setVariables(factorsAvailable);

	if (_options != NULL)
		_options->blockSignals(false);
}

void AncovaForm::termsChanged()
{
	Terms terms = _anovaModel->terms();
	terms.discardWhatDoesContainTheseComponents(_covariatesListModel->assigned());
	ui->marginalMeansTerms->setVariables(terms);

}
