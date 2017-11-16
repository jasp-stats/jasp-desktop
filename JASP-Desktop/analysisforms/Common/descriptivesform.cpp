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

#include "descriptivesform.h"
#include "ui_descriptivesform.h"

#include <boost/foreach.hpp>

#include <QDebug>

DescriptivesForm::DescriptivesForm(QWidget *parent) :
	AnalysisForm("DescriptivesForm", parent),
	ui(new Ui::DescriptivesForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *model = new TableModelVariablesAssigned(this);
	model->setSource(&_availableVariablesModel);

	ui->variables->setModel(model);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	TableModelVariablesAssigned *modelsb = new TableModelVariablesAssigned(this);
	modelsb->setSource(&_availableVariablesModel);
	modelsb->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	modelsb->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeNominalText);

	ui->splitby->setModel(modelsb);
	ui->splitby->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignSplitby->setSourceAndTarget(ui->listAvailableFields, ui->splitby);

	ui->pagePlots->setStyleSheet("QGroupBox {border:0;}");

	ui->pageStatistics->hide();
	ui->pageCharts->hide();
	ui->pageFormat->hide();
	ui->pagePlots->hide();

#ifdef QT_NO_DEBUG
	// temporarily hides until the appropriate R code is implemented
	//ui->buttonAssignSplitby->hide();
	//ui->splitPlotColour->hide();
	//ui->splitby->hide();
	//ui->splitbyLabel->hide();
	//ui->splitPlotElements->hide();
	ui->widgetCharts->hide();
	ui->widgetFormat->hide();
	ui->statisticsValuesAreGroupMidpoints->hide();
#else
	//ui->buttonAssignSplitby->setStyleSheet("background-color: pink;");
	//ui->splitPlotColour->setStyleSheet("background-color: pink;");
	//ui->splitby->setStyleSheet("background-color: pink;");
	//ui->splitbyLabel->setStyleSheet("background-color: pink;");
	//ui->splitPlotElements->setStyleSheet("background-color: pink;");
	ui->widgetCharts->setStyleSheet("background-color: pink;");
	ui->widgetFormat->setStyleSheet("background-color: pink;");
	ui->statisticsValuesAreGroupMidpoints->setStyleSheet("background-color: pink;");
#endif

}

DescriptivesForm::~DescriptivesForm()
{
	delete ui;
}
