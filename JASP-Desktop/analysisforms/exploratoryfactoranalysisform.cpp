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

#include "exploratoryfactoranalysisform.h"
#include "ui_exploratoryfactoranalysisform.h"

#include "analysisform.h"

ExploratoryFactorAnalysisForm::ExploratoryFactorAnalysisForm(QWidget *parent) :
	AnalysisForm("ExploratoryFactorAnalysisForm", parent),
	ui(new Ui::ExploratoryFactorAnalysisForm)
{
	ui->setupUi(this);
	ui->highlightText->setText("0.4");

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *variablesModel = new TableModelVariablesAssigned(this);
	variablesModel->setSource(&_availableVariablesModel);
	variablesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	variablesModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->variables->setModel(variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

/*
	TableModelVariablesAssigned *groupingVariableModel = new TableModelVariablesAssigned(this);
	groupingVariableModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	groupingVariableModel->setSource(&_availableVariablesModel);
	ui->groupingVariable->setModel(groupingVariableModel);
	ui->groupingVariable->setDoubleClickTarget(ui->listAvailableFields);
*/

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
    ui->containerOptions->hide();
//	ui->buttonAssignGroupingVariable->setSourceAndTarget(ui->listAvailableFields, ui->groupingVariable);

//	ui->confidenceIntervalInterval->setLabel("Confidence interval");
//	ui->descriptivesPlotsConfidenceInterval->setLabel("Confidence interval");

	connect(ui->highlightSlider,SIGNAL(valueChanged(int)),this,SLOT(HandleSlider(int)));
	connect(ui->highlightText,SIGNAL(textChanged(QString)),this,SLOT(HandleLineEdit(QString)));
}

/*
void ExploratoryFactorAnalysisForm::HandleSlider(double i)
{
	QString	s = QString::number((double)i/100.0, 'f', 2);
	ui->highlightText->setText(s);
}
*/
void ExploratoryFactorAnalysisForm::HandleSlider(int i)
{
	QString	s = QString::number(((double)i)/100.0, 'f', 2);
	ui->highlightText->setText(s);
    ui->highlightText->finalise();
}

void ExploratoryFactorAnalysisForm::HandleLineEdit(QString s)
{
	ui->highlightSlider->setValue(s.toDouble() * 100.0);
}


ExploratoryFactorAnalysisForm::~ExploratoryFactorAnalysisForm()
{
	delete ui;
}
