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

#include "anovarepeatedmeasuresform.h"
#include "ui_anovarepeatedmeasuresform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

using namespace std;

AnovaRepeatedMeasuresForm::AnovaRepeatedMeasuresForm(QWidget *parent) :
	AnalysisForm("AnovaRepeatedMeasuresForm", parent),
	ui(new Ui::AnovaRepeatedMeasuresForm)
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

	ui->buttonAssignRMCells->setSourceAndTarget(ui->listAvailableFields, ui->repeatedMeasuresCells);
	ui->buttonAssignBSFactors->setSourceAndTarget(ui->listAvailableFields, ui->betweenSubjectFactors);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);

	_withinSubjectsTermsModel = new TableModelAnovaModel(this);
	ui->withinModelTerms->setModel(_withinSubjectsTermsModel);
	connect(_withinSubjectsTermsModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	_betweenSubjectsTermsModel = new TableModelAnovaModel(this);
	ui->betweenModelTerms->setModel(_betweenSubjectsTermsModel);
	connect(_betweenSubjectsTermsModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	_contrastsModel = new TableModelVariablesOptions();
    ui->contrasts->setModel(_contrastsModel);

	connect(_designTableModel, SIGNAL(designChanging()), this, SLOT(factorsChanging()));
	connect(_designTableModel, SIGNAL(designChanged()), this, SLOT(withinSubjectsDesignChanged()));
	connect(_designTableModel, SIGNAL(factorAdded(Terms)), _withinSubjectsTermsModel, SLOT(addFixedFactors(Terms)));
	connect(_designTableModel, SIGNAL(factorRemoved(Terms)), _withinSubjectsTermsModel, SLOT(removeVariables(Terms)));

	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignedTo(Terms)), _betweenSubjectsTermsModel, SLOT(addFixedFactors(Terms)));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(unassigned(Terms)), _betweenSubjectsTermsModel, SLOT(removeVariables(Terms)));

	connect(_covariatesListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_covariatesListModel, SIGNAL(assignedTo(Terms)), _betweenSubjectsTermsModel, SLOT(addCovariates(Terms)));
	connect(_covariatesListModel, SIGNAL(unassigned(Terms)), _betweenSubjectsTermsModel, SLOT(removeVariables(Terms)));

	_plotFactorsAvailableTableModel = new TableModelVariablesAvailable();
	ui->plotVariables->setModel(_plotFactorsAvailableTableModel);

	_horizontalAxisTableModel = new TableModelVariablesAssigned(this);
	_horizontalAxisTableModel->setSource(_plotFactorsAvailableTableModel);
	ui->plotHorizontalAxis->setModel(_horizontalAxisTableModel);

	_seperateLinesTableModel = new TableModelVariablesAssigned(this);
	_seperateLinesTableModel->setSource(_plotFactorsAvailableTableModel);
	ui->plotSeparateLines->setModel(_seperateLinesTableModel);

	_seperatePlotsTableModel = new TableModelVariablesAssigned(this);
	_seperatePlotsTableModel->setSource(_plotFactorsAvailableTableModel);
	ui->plotSeparatePlots->setModel(_seperatePlotsTableModel);

	ui->buttonAssignHorizontalAxis->setSourceAndTarget(ui->plotVariables, ui->plotHorizontalAxis);
	ui->buttonAssignSeperateLines->setSourceAndTarget(ui->plotVariables, ui->plotSeparateLines);
	ui->buttonAssignSeperatePlots->setSourceAndTarget(ui->plotVariables, ui->plotSeparatePlots);

	ui->containerModel->hide();
	ui->containerFactors->hide();
	ui->containerOptions->hide();
	ui->containerPostHocTests->hide();
	ui->containerDescriptivesPlot->hide();
	ui->containerAssumptions->hide();

	ui->withinModelTerms->setFactorsLabel("Repeated Measures Components");
	ui->betweenModelTerms->setFactorsLabel("Between Subjects Components");

	ui->confidenceIntervalInterval->setLabel("Confidence interval");

	connect(_designTableModel, SIGNAL(designChanged()), this, SLOT(withinSubjectsDesignChanged()));

#ifdef QT_NO_DEBUG
	ui->groupCompareMainEffects->hide();

#else
	ui->groupCompareMainEffects->setStyleSheet("background-color: pink ;");

#endif
}

AnovaRepeatedMeasuresForm::~AnovaRepeatedMeasuresForm()
{
	delete ui;
}

void AnovaRepeatedMeasuresForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	Terms factors;

	foreach (const Factor &factor, _designTableModel->design())
		factors.add(factor.first);

	_withinSubjectsTermsModel->setVariables(factors);

	if (_withinSubjectsTermsModel->terms().size() == 0)
		_withinSubjectsTermsModel->addFixedFactors(factors);

	_betweenSubjectsTermsModel->setVariables(_betweenSubjectsFactorsListModel->assigned());
}

void AnovaRepeatedMeasuresForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void AnovaRepeatedMeasuresForm::factorsChanged()
{
	Terms factorsAvailable;

	foreach (const Factor &factor, _designTableModel->design())
		factorsAvailable.add(factor.first);

	factorsAvailable.add(_betweenSubjectsFactorsListModel->assigned());

	_contrastsModel->setVariables(factorsAvailable);
	_plotFactorsAvailableTableModel->setVariables(factorsAvailable);

	Terms plotVariablesAssigned;
	plotVariablesAssigned.add(_horizontalAxisTableModel->assigned());
	plotVariablesAssigned.add(_seperateLinesTableModel->assigned());
	plotVariablesAssigned.add(_seperatePlotsTableModel->assigned());
	_plotFactorsAvailableTableModel->notifyAlreadyAssigned(plotVariablesAssigned);

	ui->postHocTestsVariables->setVariables(factorsAvailable);

	if (_options != NULL)
		_options->blockSignals(false);
}

void AnovaRepeatedMeasuresForm::termsChanged()
{
	Terms terms;

	terms.add(string("~OVERALL"));

	foreach (const Factor &factor, _designTableModel->design())
		terms.add(factor.first);

	terms.add(_withinSubjectsTermsModel->terms());

	ui->marginalMeansTerms->setVariables(terms);
}

void AnovaRepeatedMeasuresForm::withinSubjectsDesignChanged()
{
	_withinSubjectCellsListModel->setDesign(_designTableModel->design());
	factorsChanged();
}

void AnovaRepeatedMeasuresForm::anovaDesignTableClicked(QModelIndex index)
{
	// the second column contains an X to delete the row

	if (index.column() == 1)
		_designTableModel->removeRow(index.row());
}
