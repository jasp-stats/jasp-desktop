//
// Copyright (C) 2013-2016 University of Amsterdam
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

#include "basregressionlinearlinkform.h"
#include "ui_basregressionlinearlinkform.h"


BASRegressionLinearLinkForm::BASRegressionLinearLinkForm(QWidget *parent) :
	AnalysisForm("BASRegressionLinearLinkForm", parent),
	ui(new Ui::BASRegressionLinearLinkForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_dependentListModel->setSource(&_availableVariablesModel);
	ui->dependent->setModel(_dependentListModel);

	_covariatesListModel = new TableModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableVariablesModel);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->covariates->setModel(_covariatesListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);

	connect(_covariatesListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	ui->advancedOptions->hide();
}

BASRegressionLinearLinkForm::~BASRegressionLinearLinkForm()
{
	delete ui;
}

void BASRegressionLinearLinkForm:: bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);
}

void BASRegressionLinearLinkForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void BASRegressionLinearLinkForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}
