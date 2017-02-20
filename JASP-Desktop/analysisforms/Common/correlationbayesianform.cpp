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

#include "correlationbayesianform.h"
#include "ui_correlationbayesianform.h"

CorrelationBayesianForm::CorrelationBayesianForm(QWidget *parent) :
	AnalysisForm("CorrelationBayesianForm", parent),
	ui(new Ui::CorrelationBayesianForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
    _availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->availableVariables->setModel(&_availableVariablesModel);
	ui->availableVariables->setDoubleClickTarget(ui->variables);

	_modelVariables = new TableModelVariablesAssigned();
	_modelVariables->setSource(&_availableVariablesModel);
	_modelVariables->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	_modelVariables->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	ui->variables->setModel(_modelVariables);

	ui->assignButton->setSourceAndTarget(ui->availableVariables, ui->variables);

	ui->missingValues->hide();

#ifdef QT_NO_DEBUG
	ui->spearman->hide();
    //ui->kendallsTauB->hide();
    ui->ciValueContainer->hide();
    //ui->credibleInterval->hide();
#else
	ui->spearman->setStyleSheet("background-color: pink ;");
    //ui->kendallsTauB->setStyleSheet("background-color: pink ;");
    ui->ciValueContainer->setStyleSheet("background-color: pink ;");
    //ui->credibleInterval->setStyleSheet("background-color: pink ;");
#endif

    ui->ciValue->setLabel("Credible interval");
}

CorrelationBayesianForm::~CorrelationBayesianForm()
{
	delete ui;
}
