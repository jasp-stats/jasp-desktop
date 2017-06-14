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

#include "mlregressionknnform.h"
#include "ui_mlregressionknnform.h"

MLRegressionKNNForm::MLRegressionKNNForm(QWidget *parent) :
	AnalysisForm("MLRegressionKNNForm", parent),
	ui(new Ui::MLRegressionKNNForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_targetListModel = new TableModelVariablesAssigned(this);
	_targetListModel->setSource(&_availableVariablesModel);
    _targetListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
    _targetListModel->setVariableTypesAllowed(Column::ColumnTypeScale);
	ui->target->setModel(_targetListModel);

	_predictorsListModel = new TableModelVariablesAssigned(this);
	_predictorsListModel->setSource(&_availableVariablesModel);
    _predictorsListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	ui->predictors->setModel(_predictorsListModel);

	_indicatorListModel = new TableModelVariablesAssigned(this);
	_indicatorListModel->setSource(&_availableVariablesModel);
    _indicatorListModel->setVariableTypesSuggested(Column::ColumnTypeNominal);
    _indicatorListModel->setVariableTypesAllowed(Column::ColumnTypeNominal);
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

MLRegressionKNNForm::~MLRegressionKNNForm()
{
	delete ui;
}

void MLRegressionKNNForm::defaultOptions()
{
	QSizePolicy retain = ui->trainingDataManual->sizePolicy();
	retain.setRetainSizeWhenHidden(true);

	ui->trainingDataManual->setSizePolicy(retain);
	ui->trainingDataManual->hide();

	ui->nearestNeighboursCount->setSizePolicy(retain);
	ui->nearestNeighboursCount->hide();

	ui->optimizedFrom->setSizePolicy(retain);
	ui->optimizedFrom->hide();

	ui->optimizedTo->setSizePolicy(retain);
	ui->optimizedTo->hide();

	ui->distanceParameterManual->setSizePolicy(retain);
	ui->distanceParameterManual->hide();

	retain = ui->label_to->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->label_to->setSizePolicy(retain);
	ui->label_to->hide();
}

void MLRegressionKNNForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	_anovaModel->setVariables(_targetListModel->assigned(), _predictorsListModel->assigned(), _indicatorListModel->assigned());

	factorsChanged();
}

void MLRegressionKNNForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void MLRegressionKNNForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}

void MLRegressionKNNForm::on_manual_1_clicked(bool checked)
{
	if (checked) {
		ui->trainingDataManual->show();
	}
}

void MLRegressionKNNForm::on_auto_1_clicked(bool checked)
{
	if (checked) {
		ui->trainingDataManual->hide();
	}
}

void MLRegressionKNNForm::on_manual_2_clicked(bool checked)
{
	if (checked) {
		ui->nearestNeighboursCount->show();
		ui->optimizedFrom->hide();
		ui->optimizedTo->hide();
		ui->label_to->hide();
	}
}

void MLRegressionKNNForm::on_auto_2_clicked(bool checked)
{
	if (checked) {
		ui->nearestNeighboursCount->hide();
		ui->optimizedFrom->hide();
		ui->optimizedTo->hide();
		ui->label_to->hide();
	}
}

void MLRegressionKNNForm::on_optimized_2_clicked(bool checked)
{
	if (checked) {
		ui->nearestNeighboursCount->hide();
		ui->optimizedFrom->show();
		ui->optimizedTo->show();
		ui->label_to->show();
	}
}

void MLRegressionKNNForm::on_manual_3_clicked(bool checked)
{
	if (checked) {
		ui->distanceParameterManual->show();
	}
}

void MLRegressionKNNForm::on_auto_3_clicked(bool checked)
{
	if (checked) {
		ui->distanceParameterManual->hide();
	}
}

void MLRegressionKNNForm::on_optimized_3_clicked(bool checked)
{
	if (checked) {
		ui->distanceParameterManual->hide();
	}
}
