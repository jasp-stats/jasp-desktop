//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "ancovabayesianform.h"
#include "ui_ancovabayesianform.h"

AncovaBayesianForm::AncovaBayesianForm(QWidget *parent) :
	AnalysisForm("AncovaBayesianForm", parent),
	ui(new Ui::AncovaBayesianForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
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
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->covariates->setModel(_covariatesListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->randomFactors);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);

	_anovaModel = new TableModelAnovaModel(this);
	_anovaModel->setPiecesCanBeAssigned(false);
	ui->modelTerms->setModel(_anovaModel);
	
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

	ui->modelTerms->hide();
	ui->containerDescriptivesPlot->hide();
	ui->advancedOptions->hide();

	ui->plotCredibleIntervalInterval->setLabel("Credible interval");
	ui->priorFixedEffects->setLabel("r scale fixed effects");
	ui->priorRandomEffects->setLabel("r scale random effects");
	ui->priorCovariates->setLabel("r scale covariates");

#ifdef QT_DEBUG
	ui->widgetPosteriorOptions->setStyleSheet("QWidget { background-color: pink; }");
	ui->posteriorEstimates->setStyleSheet("QWidget { background-color: pink; }");
#else
	ui->widgetPosteriorOptions->hide();
	ui->posteriorEstimates->hide();
#endif
}

AncovaBayesianForm::~AncovaBayesianForm()
{
	delete ui;
}

void AncovaBayesianForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	_anovaModel->setVariables(_fixedFactorsListModel->assigned(), _randomFactorsListModel->assigned(), _covariatesListModel->assigned());

	factorsChanged();
}

void AncovaBayesianForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void AncovaBayesianForm::factorsChanged()
{
	Terms factorsAvailable;

	factorsAvailable.add(_fixedFactorsListModel->assigned());
	factorsAvailable.add(_randomFactorsListModel->assigned());

	_plotFactorsAvailableTableModel->setVariables(factorsAvailable);

	Terms plotVariablesAssigned;
	plotVariablesAssigned.add(_horizontalAxisTableModel->assigned());
	plotVariablesAssigned.add(_seperateLinesTableModel->assigned());
	plotVariablesAssigned.add(_seperatePlotsTableModel->assigned());
	_plotFactorsAvailableTableModel->notifyAlreadyAssigned(plotVariablesAssigned);
	
	if (_options != NULL)
		_options->blockSignals(false);
}

