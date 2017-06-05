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

#include "multinomialtestform.h"
#include "ui_multinomialtestform.h"

MultinomialTestForm::MultinomialTestForm(QWidget *parent) :
	AnalysisForm("MultinomialTestForm", parent),
	ui(new Ui::MultinomialTestForm)
{
	ui->setupUi(this);

	ui->listAvailableVariables->setModel(&_availableVariablesModel);
	ui->listAvailableVariables->setDoubleClickTarget(ui->factor);

	TableModelVariablesAssigned *factorModel = new TableModelVariablesAssigned(this);
	factorModel->setSource(&_availableVariablesModel);
	factorModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	factorModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeNominalText);

	ui->factor->setModel(factorModel);
	ui->factor->setDoubleClickTarget(ui->listAvailableVariables);
	ui->assignFactor->setSourceAndTarget(ui->listAvailableVariables, ui->factor);

	TableModelVariablesAssigned *countModel = new TableModelVariablesAssigned(this);
	countModel->setSource(&_availableVariablesModel);
	countModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	countModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeScale);

	ui->counts->setModel(countModel);
	ui->counts->setDoubleClickTarget(ui->listAvailableVariables);
	ui->assignCounts->setSourceAndTarget(ui->listAvailableVariables, ui->counts);


	TableModelVariablesAssigned *probVarModel = new TableModelVariablesAssigned(this);
	probVarModel->setSource(&_availableVariablesModel);
	probVarModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	probVarModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeScale);

	ui->exProbVar->setModel(probVarModel);
	ui->exProbVar->setDoubleClickTarget(ui->listAvailableVariables);
	ui->assignExProbVar->setSourceAndTarget(ui->listAvailableVariables, ui->exProbVar);

	ui->tableView->setVisible(false);


#ifdef QT_NO_DEBUG

#else

#endif


}

MultinomialTestForm::~MultinomialTestForm()
{
	delete ui;
}
