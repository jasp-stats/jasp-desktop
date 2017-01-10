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

#include "anovaonewayform.h"
#include "ui_anovaonewayform.h"

#include "analysisform.h"

AnovaOneWayForm::AnovaOneWayForm(QWidget *parent) :
	AnalysisForm("AnovaOneWayForm", parent),
	ui(new Ui::AnovaOneWayForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	_variablesModel.setSource(&_availableVariablesModel);
	_variablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_variablesModel.setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->variables->setModel(&_variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	_groupingVariableModel.setSource(&_availableVariablesModel);
	_groupingVariableModel.setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_groupingVariableModel.setVariableTypesAllowed(Column::ColumnTypeNominalText | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->groupingVariable->setModel(&_groupingVariableModel);
	ui->groupingVariable->setDoubleClickTarget(ui->listAvailableFields);
	connect(&_groupingVariableModel, SIGNAL(assignmentsChanged()), this, SLOT(groupingVariableChanged()));

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignGroupingVariable->setSourceAndTarget(ui->listAvailableFields, ui->groupingVariable);

	ui->contrasts->setModel(&_contrastsModel);
	ui->contrasts->horizontalHeader()->setVisible(true);

	ui->panelContrasts->hide();
	ui->panelPostHoc->hide();
	ui->panelOptions->hide();

	connect(ui->contrasts, SIGNAL(clicked(QModelIndex)), this, SLOT(contrastsClicked(QModelIndex)));
}

AnovaOneWayForm::~AnovaOneWayForm()
{
	delete ui;
}

void AnovaOneWayForm::groupingVariableChanged()
{
	Terms terms = _groupingVariableModel.assigned();
	if (terms.size() > 0)
	{
		QVariant value = this->requestInfo(terms.at(0), VariableInfo::Labels);
		Terms labels(value.toStringList());
		_contrastsModel.setLabels(labels);
	}
	else
	{
		_contrastsModel.setLabels(Terms());
	}
}

void AnovaOneWayForm::contrastsClicked(QModelIndex index)
{
	if (index.column() == _contrastsModel.columnCount() - 1)
	{
		_contrastsModel.setData(index, "A");
	}
	else if (index.column() > 0)
	{
		QString value = _contrastsModel.data(index).toString();

		if (value == "")
			_contrastsModel.setData(index, "A");
		else if (value == "A")
			_contrastsModel.setData(index, "B");
		else
			_contrastsModel.setData(index, "");
	}
}
