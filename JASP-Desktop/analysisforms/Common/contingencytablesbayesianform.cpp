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

#include "contingencytablesbayesianform.h"
#include "ui_contingencytablesbayesianform.h"

ContingencyTablesBayesianForm::ContingencyTablesBayesianForm(QWidget *parent) :
	AnalysisForm("ContingencyTablesBayesianForm", parent),
	ui(new Ui::ContingencyTablesBayesianForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_rowsModel = new TableModelVariablesAssigned();
	_rowsModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	_rowsModel->setSource(&_availableVariablesModel);
	ui->rows->setModel(_rowsModel);

	_columnsModel = new TableModelVariablesAssigned();
	_columnsModel->setSource(&_availableVariablesModel);
	_columnsModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->columns->setModel(_columnsModel);

	_countsModel = new TableModelVariablesAssigned();
	_countsModel->setSource(&_availableVariablesModel);
	_countsModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_countsModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->counts->setModel(_countsModel);

	_layersModel = new TableModelVariablesLevels();
	_layersModel->setSource(&_availableVariablesModel);
	_layersModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->layers->setModel(_layersModel);

	ui->buttonAssignRows->setSourceAndTarget(ui->listAvailableFields, ui->rows);
	ui->buttonAssignColumns->setSourceAndTarget(ui->listAvailableFields, ui->columns);
	ui->buttonAssignCounts->setSourceAndTarget(ui->listAvailableFields, ui->counts);
	ui->buttonAssignLayers->setSourceAndTarget(ui->listAvailableFields, ui->layers);

	ui->panelStatistics->hide();
	ui->panelOptions->hide();

	ui->oddsRatioCredibleIntervalInterval->setLabel("Credible interval");
	ui->effectSizeCredibleIntervalInterval->setLabel("Credible interval");
	ui->priorConcentration->setLabel("Prior concentration");

#ifdef QT_NO_DEBUG
	ui->effectSize->hide();
	ui->effectSizeCredibleIntervalContainer->hide();
	ui->plotPosteriorEffectSize->hide();
#else
	ui->effectSize->setStyleSheet("background-color: pink ;");
	ui->effectSizeCredibleIntervalContainer->setStyleSheet("background-color: pink ;");
	ui->plotPosteriorEffectSize->setStyleSheet("background-color: pink ;");
#endif
}

ContingencyTablesBayesianForm::~ContingencyTablesBayesianForm()
{
	delete ui;
}

void ContingencyTablesBayesianForm::otherSamplingToggled(bool on)
{
	if (on)
		ui->hypothesis->setEnabled(false);
}

void ContingencyTablesBayesianForm::independentMultinomialSamplingToggled(bool on)
{
	if (on)
		ui->hypothesis->setEnabled(true);
}
