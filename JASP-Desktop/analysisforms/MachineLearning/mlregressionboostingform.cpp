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

#include "mlregressionboostingform.h"
#include "ui_mlregressionboostingform.h"

MLRegressionBoostingForm::MLRegressionBoostingForm(QWidget *parent) :
	AnalysisForm("MLRegressionBoostingForm", parent),
	ui(new Ui::MLRegressionBoostingForm)
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

	connect(_targetListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_targetListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	connect(_predictorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_predictorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	connect(_indicatorListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_indicatorListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	ui->advancedOptions->hide();

	defaultOptions();
}

MLRegressionBoostingForm::~MLRegressionBoostingForm()
{
	delete ui;
}

void MLRegressionBoostingForm::defaultOptions()
{

}

void MLRegressionBoostingForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	factorsChanged();
}

void MLRegressionBoostingForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void MLRegressionBoostingForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}

void MLRegressionBoostingForm::on_plotPredictivePerformance_clicked(bool checked)
{
        ui->oneWay->setEnabled(true);
}
