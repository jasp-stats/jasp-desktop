//
// Copyright (C) 2017 University of Amsterdam
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

#include "mlregressionrandomforestform.h"
#include "ui_mlregressionrandomforestform.h"

MLRegressionRandomForestForm::MLRegressionRandomForestForm(QWidget *parent) :
	AnalysisForm("MLRegressionRandomForestForm", parent),
	ui(new Ui::MLRegressionRandomForestForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_targetListModel = new TableModelVariablesAssigned(this);
	_targetListModel->setSource(&_availableVariablesModel);
	_targetListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->target->setModel(_targetListModel);

	_predictorsListModel = new TableModelVariablesAssigned(this);
	_predictorsListModel->setSource(&_availableVariablesModel);
	_predictorsListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->predictors->setModel(_predictorsListModel);

	_indicatorListModel = new TableModelVariablesAssigned(this);
	_indicatorListModel->setSource(&_availableVariablesModel);
	_indicatorListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_indicatorListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->indicator->setModel(_indicatorListModel);

	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->target);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->predictors);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->indicator);

	_anovaModel = new TableModelAnovaModel(this);
	_anovaModel->setPiecesCanBeAssigned(false);
	ui->modelTerms->setModel(_anovaModel);
	ui->modelTerms->hide();

	connect(_targetListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_targetListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_targetListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addFixedFactors(Terms)));
	connect(_targetListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	connect(_predictorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_predictorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_predictorsListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addRandomFactors(Terms)));
	connect(_predictorsListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	connect(_indicatorListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_indicatorListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_indicatorListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addCovariates(Terms)));
	connect(_indicatorListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	ui->advancedOptions->hide();

	defaultOptions();
}

MLRegressionRandomForestForm::~MLRegressionRandomForestForm()
{
	delete ui;
}

void MLRegressionRandomForestForm::defaultOptions()
{
	ui->auto_1->setChecked(true);
	ui->auto_2->setChecked(true);
	ui->auto_3->setChecked(true);
	ui->auto_4->setChecked(true);
	ui->auto_5->setChecked(true);
	ui->auto_6->setChecked(true);
	ui->auto_7->setChecked(true);

	QSizePolicy retain = ui->numberOfTrees->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->numberOfTrees->setSizePolicy(retain);

	ui->numberOfTrees->hide();

	ui->numberOfPredictors->setSizePolicy(retain);
	ui->numberOfPredictors->hide();

	ui->percentageDataBootstrap->setSizePolicy(retain);
	ui->percentageDataBootstrap->hide();

	ui->percentageDataTraining->setSizePolicy(retain);
	ui->percentageDataTraining->hide();

	ui->modelMinimumTerminalNode->setSizePolicy(retain);
	ui->modelMinimumTerminalNode->hide();

	ui->modelMaximumTerminalNode->setSizePolicy(retain);
	ui->modelMaximumTerminalNode->hide();

	ui->seed->setSizePolicy(retain);
	ui->seed->hide();
}

void MLRegressionRandomForestForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	_anovaModel->setVariables(_targetListModel->assigned(), _predictorsListModel->assigned(), _indicatorListModel->assigned());

	factorsChanged();
}

void MLRegressionRandomForestForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void MLRegressionRandomForestForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}

void MLRegressionRandomForestForm::on_manual_1_clicked(bool checked)
{
	if (checked) {
		ui->numberOfTrees->show();
	}
}

void MLRegressionRandomForestForm::on_auto_1_clicked(bool checked)
{
	if (checked) {
		ui->numberOfTrees->hide();
	}
}

void MLRegressionRandomForestForm::on_manual_2_clicked(bool checked)
{
	if (checked) {
		ui->numberOfPredictors->show();
	}
}

void MLRegressionRandomForestForm::on_auto_2_clicked(bool checked)
{
	if (checked) {
		ui->numberOfPredictors->hide();
	}
}

void MLRegressionRandomForestForm::on_manual_3_clicked(bool checked)
{
	if (checked) {
		ui->percentageDataBootstrap->show();
	}
}

void MLRegressionRandomForestForm::on_auto_3_clicked(bool checked)
{
	if (checked) {
		ui->percentageDataBootstrap->hide();
	}
}

void MLRegressionRandomForestForm::on_manual_4_clicked(bool checked)
{
	if (checked) {
		ui->percentageDataTraining->show();
	}
}

void MLRegressionRandomForestForm::on_auto_4_clicked(bool checked)
{
	if (checked) {
		ui->percentageDataTraining->hide();
	}
}

void MLRegressionRandomForestForm::on_manual_5_clicked(bool checked)
{
	if (checked) {
		ui->seed->show();
	}
}

void MLRegressionRandomForestForm::on_auto_5_clicked(bool checked)
{
	if (checked) {
		ui->seed->hide();
	}
}

void MLRegressionRandomForestForm::on_manual_6_clicked(bool checked)
{
	if (checked) {
		ui->modelMinimumTerminalNode->show();
	}
}

void MLRegressionRandomForestForm::on_auto_6_clicked(bool checked)
{
	if (checked) {
		ui->modelMinimumTerminalNode->hide();
	}
}

void MLRegressionRandomForestForm::on_manual_7_clicked(bool checked)
{
	if (checked) {
		ui->modelMaximumTerminalNode->show();
	}
}

void MLRegressionRandomForestForm::on_auto_7_clicked(bool checked)
{
	if (checked) {
		ui->modelMaximumTerminalNode->hide();
	}
}
