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

#include "regressionloglinearform.h"
#include "ui_regressionloglinearform.h"

RegressionLogLinearForm::RegressionLogLinearForm(QWidget *parent) :
	AnalysisForm("RegressionLogLinearForm", parent),
	ui(new Ui::RegressionLogLinearForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_countsModel = new TableModelVariablesAssigned();
	_countsModel->setSource(&_availableVariablesModel);
	_countsModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_countsModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->counts->setModel(_countsModel);

	_factorsModel = new TableModelVariablesAssigned();
	_factorsModel->setSource(&_availableVariablesModel);
	_factorsModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->factors->setModel(_factorsModel);

	ui->buttonAssignCounts->setSourceAndTarget(ui->listAvailableFields, ui->counts);
	ui->buttonAssignFactors->setSourceAndTarget(ui->listAvailableFields, ui->factors);

	_model = new TableModelAnovaModel(this);
	_model->setPiecesCanBeAssigned(false);
	ui->modelTerms->setModel(_model);
	ui->modelTerms->hide();

	connect(_factorsModel, SIGNAL(assignedTo(Terms)), _model, SLOT(addFixedFactors(Terms)));
	connect(_factorsModel, SIGNAL(unassigned(Terms)), _model, SLOT(removeVariables(Terms)));

	ui->panelStatistics->hide();

#ifdef QT_NO_DEBUG

#else

#endif
}

RegressionLogLinearForm::~RegressionLogLinearForm()
{
	delete ui;
}

void RegressionLogLinearForm:: bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);
	_model->setVariables(Terms(), Terms(), _factorsModel->assigned());
}
