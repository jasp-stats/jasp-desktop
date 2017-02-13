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

#include "bainanovarepeatedmeasuresbayesianform.h"
#include "ui_bainanovarepeatedmeasuresbayesianform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

BainAnovaRepeatedMeasuresBayesianForm::BainAnovaRepeatedMeasuresBayesianForm(QWidget *parent) :
	AnalysisForm("BainAnovaRepeatedMeasuresBayesianForm", parent),
	ui(new Ui::BainAnovaRepeatedMeasuresBayesianForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_designTableModel = new TableModelAnovaDesign(this);
	ui->repeatedMeasuresFactors->setModel(_designTableModel);

	// this is a hack to allow deleting factors and levels :/
	// ideally this would be handled between the TableView and the model
	// and wouldn't require the surrounding classes' intervention like this
	connect(ui->repeatedMeasuresFactors, SIGNAL(clicked(QModelIndex)), this, SLOT(anovaDesignTableClicked(QModelIndex)));

	_withinSubjectCellsListModel = new TableModelAnovaWithinSubjectCells(this);
	_withinSubjectCellsListModel->setSource(&_availableVariablesModel);
	_withinSubjectCellsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_withinSubjectCellsListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->repeatedMeasuresCells->setModel(_withinSubjectCellsListModel);
	ui->repeatedMeasuresCells->viewport()->setAttribute(Qt::WA_Hover);
	ui->repeatedMeasuresCells->setItemDelegate(new AnovaHoverDelegate(ui->repeatedMeasuresCells));

	_betweenSubjectsFactorsListModel = new TableModelVariablesAssigned(this);
	_betweenSubjectsFactorsListModel->setSource(&_availableVariablesModel);
	_betweenSubjectsFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->betweenSubjectFactors->setModel(_betweenSubjectsFactorsListModel);

	_covariatesListModel = new TableModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableVariablesModel);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->covariates->setModel(_covariatesListModel);

	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->repeatedMeasuresCells);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->betweenSubjectFactors);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);

	_anovaModel = new TableModelAnovaModel(this);
	_anovaModel->setPiecesCanBeAssigned(false);
	ui->modelTerms->setModel(_anovaModel);

	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addFixedFactors(Terms)));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	connect(_covariatesListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_covariatesListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addCovariates(Terms)));
	connect(_covariatesListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	connect(_designTableModel, SIGNAL(designChanging()), this, SLOT(factorsChanging()));
	connect(_designTableModel, SIGNAL(designChanged()), this, SLOT(withinSubjectsDesignChanged()));
	connect(_designTableModel, SIGNAL(factorAdded(Terms)), _anovaModel, SLOT(addFixedFactors(Terms)));
	connect(_designTableModel, SIGNAL(factorRemoved(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	ui->containerModel->hide();
	ui->advancedOptions->hide();

	ui->priorFixedEffects->setLabel("r scale fixed effects");
	ui->priorRandomEffects->setLabel("r scale random effects");
	ui->priorCovariates->setLabel("r scale covariates");
}

BainAnovaRepeatedMeasuresBayesianForm::~BainAnovaRepeatedMeasuresBayesianForm()
{
	delete ui;
}

void BainAnovaRepeatedMeasuresBayesianForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	Terms factorsAvailable;

	foreach (const Factor &factor, _designTableModel->design())
		factorsAvailable.add(factor.first);

	factorsAvailable.add(_betweenSubjectsFactorsListModel->assigned());

	_anovaModel->setVariables(factorsAvailable);
}

void BainAnovaRepeatedMeasuresBayesianForm::withinSubjectsDesignChanged()
{
	_withinSubjectCellsListModel->setDesign(_designTableModel->design());

	factorsChanged();
}

void BainAnovaRepeatedMeasuresBayesianForm::anovaDesignTableClicked(QModelIndex index)
{
	// the second column contains an X to delete the row

	if (index.column() == 1)
		_designTableModel->removeRow(index.row());
}

void BainAnovaRepeatedMeasuresBayesianForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void BainAnovaRepeatedMeasuresBayesianForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}
