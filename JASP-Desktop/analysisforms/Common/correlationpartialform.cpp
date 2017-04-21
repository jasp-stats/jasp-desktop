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

#include "correlationpartialform.h"
#include "ui_correlationpartialform.h"

CorrelationPartialForm::CorrelationPartialForm(QWidget *parent) :
	AnalysisForm("CorrelationPartialForm", parent),
	ui(new Ui::CorrelationPartialForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	_modelVariables = new TableModelVariablesAssigned();
	_modelVariables->setSource(&_availableVariablesModel);
	ui->variables->setModel(_modelVariables);

	_modelControllingFor = new TableModelVariablesAssigned();
	_modelControllingFor->setSource(&_availableVariablesModel);
	ui->controllingFor->setModel(_modelControllingFor);

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignControllingFor->setSourceAndTarget(ui->listAvailableFields, ui->controllingFor);

	ui->panelOptions->hide();
}

CorrelationPartialForm::~CorrelationPartialForm()
{
	delete ui;
}
