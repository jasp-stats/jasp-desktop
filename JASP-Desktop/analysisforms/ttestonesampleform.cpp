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

#include "ttestonesampleform.h"
#include "ui_ttestonesampleform.h"

#include "analysisform.h"
#include "widgets/tablemodelvariablesassigned.h"

TTestOneSampleForm::TTestOneSampleForm(QWidget *parent) :
	AnalysisForm("TTestOneSampleForm", parent),
	ui(new Ui::TTestOneSampleForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *variablesModel = new TableModelVariablesAssigned(this);
	variablesModel->setSource(&_availableVariablesModel);
	variablesModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	variablesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->variables);

	ui->confidenceIntervalInterval->setLabel("Confidence interval");
	ui->descriptivesPlotsConfidenceInterval->setLabel("Confidence interval");

#ifdef QT_NO_DEBUG
	// temporarily hides until the appropriate R code is implemented


#else


#endif

}

TTestOneSampleForm::~TTestOneSampleForm()
{
	delete ui;
}
