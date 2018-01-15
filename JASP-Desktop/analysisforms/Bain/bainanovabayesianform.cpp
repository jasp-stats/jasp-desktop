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

#include "bainanovabayesianform.h"
#include "ui_bainanovabayesianform.h"

#include "widgets/itemmodelselectvariable.h"


BainAnovaBayesianForm::BainAnovaBayesianForm(QWidget *parent) :
	AnalysisForm("BainAnovaBayesianForm", parent),
	ui(new Ui::BainAnovaBayesianForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setSource(&_availableVariablesModel);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
    _dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale);
	ui->dependent->setModel(_dependentListModel);

	_fixedFactorsListModel = new TableModelVariablesAssigned(this);
	_fixedFactorsListModel->setSource(&_availableVariablesModel);
	_fixedFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
    _fixedFactorsListModel->setVariableTypesAllowed(Column::ColumnTypeNominalText | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->fixedFactors->setModel(_fixedFactorsListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);

	ItemModelSelectVariable *itemSelectModel = new ItemModelSelectVariable(this);
	itemSelectModel->setSource(&_availableVariablesModel);

	ui->model_constraints->hide();

#ifndef JASP_DEBUG
    ui->bayesFactorType->hide();
    ui->logScale->hide();
#else
    ui->bayesFactorType->setStyleSheet("background-color: pink;");
    ui->logScale->setStyleSheet("background-color: pink;");
#endif

    connect(_fixedFactorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

}

BainAnovaBayesianForm::~BainAnovaBayesianForm()
{
	delete ui;
}

void BainAnovaBayesianForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	factorsChanged();
}

void BainAnovaBayesianForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void BainAnovaBayesianForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}
