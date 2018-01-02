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

#include "contingencytablesform.h"
#include "ui_contingencytablesform.h"

ContingencyTablesForm::ContingencyTablesForm(QWidget *parent) :
	AnalysisForm("ContingencyTables", parent),
	ui(new Ui::ContingencyTablesForm)
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
	ui->panelCells->hide();
	ui->panelOptions->hide();

#ifdef QT_NO_DEBUG
	ui->lambda->hide();
	ui->uncertaintyCoefficient->hide();

	ui->somersD->hide();
	ui->kendallsTauC->hide();

	ui->byIntervalEta->hide();
	ui->cochransAndMantel->hide();

	ui->hideSmallCounts->hide();
	ui->hideSmallCountsLessThan->hide();
	ui->hideSmallCountsLessThanLabel->hide();

	ui->groupZTest->hide();
	ui->groupResiduals->hide();

	ui->groupCochrans->hide();
	ui->groupNominalByInterval->hide();

#else
	ui->lambda->setStyleSheet("background-color: pink;");
	ui->uncertaintyCoefficient->setStyleSheet("background-color: pink;");

	ui->somersD->setStyleSheet("background-color: pink;");
	ui->kendallsTauC->setStyleSheet("background-color: pink;");

	ui->byIntervalEta->setStyleSheet("background-color: pink;");
	ui->cochransAndMantel->setStyleSheet("background-color: pink;");

	ui->hideSmallCounts->setStyleSheet("background-color: pink;");
	ui->hideSmallCountsLessThan->setStyleSheet("background-color: pink;");
	ui->hideSmallCountsLessThanLabel->setStyleSheet("background-color: pink;");

	ui->groupZTest->setStyleSheet("background-color: pink;");
	ui->groupResiduals->setStyleSheet("background-color: pink;");

	ui->groupCochrans->setStyleSheet("background-color: pink;");
	ui->groupNominalByInterval->setStyleSheet("background-color: pink;");

#endif

	ui->oddsRatioConfidenceIntervalInterval->setLabel("Confidence interval");
}

ContingencyTablesForm::~ContingencyTablesForm()
{
	delete ui;
}
