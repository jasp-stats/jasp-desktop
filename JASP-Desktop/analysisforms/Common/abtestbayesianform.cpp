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

	ui->listAvailableVariables->setModel(&_availableVariablesModel);
	ui->listAvailableVariables->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *model = new TableModelVariablesAssigned(this);
	model->setSource(&_availableVariablesModel);

	ui->variables->setModel(model);
	ui->variables->setDoubleClickTarget(ui->listAvailableVariables);
	ui->assignButton->setSourceAndTarget(ui->listAvailableVariables, ui->variables);

	ui->advancedOptions->hide();
}

ABTestBayesianForm::~ABTestBayesianForm()
{
	delete ui;
}
