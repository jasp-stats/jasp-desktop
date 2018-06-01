//
// Copyright (C) 2018 University of Amsterdam
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

#include "abtestbayesianform.h"
#include "ui_abtestbayesianform.h"

ABTestBayesianForm::ABTestBayesianForm(QWidget *parent) :
	AnalysisForm("ABTestBayesianForm", parent),
	ui(new Ui::ABTestBayesianForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->listAvailableVariables->setModel(&_availableVariablesModel);

	_n1Model = new TableModelVariablesAssigned(this);
	_n1Model->setVariableTypesSuggested(Column::ColumnTypeScale);
	_n1Model->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_n1Model->setSource(&_availableVariablesModel);
	ui->n1->setModel(_n1Model);

	_n2Model = new TableModelVariablesAssigned(this);
	_n2Model->setVariableTypesSuggested(Column::ColumnTypeScale);
	_n2Model->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_n2Model->setSource(&_availableVariablesModel);
	ui->n2->setModel(_n2Model);

	_y1Model = new TableModelVariablesAssigned(this);
	_y1Model->setVariableTypesSuggested(Column::ColumnTypeScale);
	_y1Model->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_y1Model->setSource(&_availableVariablesModel);
	ui->y1->setModel(_y1Model);

	_y2Model = new TableModelVariablesAssigned();
	_y2Model->setSource(&_availableVariablesModel);
	_y2Model->setVariableTypesSuggested(Column::ColumnTypeScale);
	_y2Model->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->y2->setModel(_y2Model);

	ui->buttonAssign_n1->setSourceAndTarget(ui->listAvailableVariables, ui->n1);
	ui->buttonAssign_y1->setSourceAndTarget(ui->listAvailableVariables, ui->y1);
	ui->buttonAssign_n2->setSourceAndTarget(ui->listAvailableVariables, ui->n2);
	ui->buttonAssign_y2->setSourceAndTarget(ui->listAvailableVariables, ui->y2);

	ui->advancedOptions->hide();
}

ABTestBayesianForm::~ABTestBayesianForm()
{
	delete ui;
}
