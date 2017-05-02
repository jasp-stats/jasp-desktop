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

#include "correlationbayesianpairsform.h"
#include "ui_correlationbayesianpairsform.h"

#include "widgets/tablemodelvariablesassigned.h"

CorrelationBayesianPairsForm::CorrelationBayesianPairsForm(QWidget *parent) :
	AnalysisForm("CorrelationBayesianPairsForm", parent),
	ui(new Ui::CorrelationBayesianPairsForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setSupportedDropActions(Qt::MoveAction);
	_availableVariablesModel.setSupportedDragActions(Qt::CopyAction);
	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);

	ui->availableFields->setModel(&_availableVariablesModel);
	ui->availableFields->setDefaultDropAction(Qt::MoveAction);
	ui->availableFields->setDoubleClickTarget(ui->pairs);

	TableModelPairsAssigned *model = new TableModelPairsAssigned(this);
	model->setSource(&_availableVariablesModel);
	model->setVariableTypesSuggested(Column::ColumnTypeScale);
	model->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->pairs->setModel(model);

	ui->assignButton->setSourceAndTarget(ui->availableFields, ui->pairs);

#ifdef QT_NO_DEBUG
	ui->plotSequentialAnalysisRobustness->hide();
    ui->ciValueContainer->hide();
#else
	ui->plotSequentialAnalysisRobustness->setStyleSheet("background-color: pink;");
    ui->ciValueContainer->setStyleSheet("background-color: pink;");
#endif
	ui->priorWidth->setLabel("Beta* prior width");
}

CorrelationBayesianPairsForm::~CorrelationBayesianPairsForm()
{
	delete ui;
}
