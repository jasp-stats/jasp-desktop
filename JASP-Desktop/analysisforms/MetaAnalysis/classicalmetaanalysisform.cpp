//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "classicalmetaanalysisform.h"
#include "ui_classicalmetaanalysisform.h"

ClassicalMetaAnalysisForm::ClassicalMetaAnalysisForm(QWidget *parent) :
        AnalysisForm("ClassicalMetaAnalysisForm", parent),
        ui(new Ui::ClassicalMetaAnalysisForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&this->_availableVariablesModel);

	_dependentModel = new TableModelVariablesAssigned();
	_dependentModel->setSource(&_availableVariablesModel);
	_dependentModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->dependent->setModel(_dependentModel);

    // QString methods0 = "FE,DL,HE,SJ,ML,REML,EB,HS"; // full is "FE,DL,HE,SJ,ML,REML,EB,HS,GENQ", but GENQ needs weights UI element that isn't yet implemented...;
    QString methods0 = "Fixed Effects,Maximum Likelihood,Restricted ML,DerSimonian-Laird,Hedges,Hunter-Schmidt,Sidik-Jonkman,Empirical Bayes,Paule-Mandel";
    //QStringList methods = (QStringList << "FE" << "DL" << "HE" << "SJ" << "ML" << "REML" << "EB" << "HS" << "GENQ");
    QStringList methods = methods0.split(",");
    ui->method->addItems(methods);

	_covariatesModel = new TableModelVariablesAssigned();
	_covariatesModel->setSource(&_availableVariablesModel);
	_covariatesModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	_covariatesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	ui->covariates->setModel(_covariatesModel);

	_factorsModel = new TableModelVariablesAssigned();
	_factorsModel->setSource(&_availableVariablesModel);
	_factorsModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->factors->setModel(_factorsModel);

	_wlsWeightsModel = new TableModelVariablesAssigned();
	_wlsWeightsModel->setSource(&_availableVariablesModel);
	_wlsWeightsModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_wlsWeightsModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->wlsWeights->setModel(_wlsWeightsModel);

    _studyLabelModel = new TableModelVariablesAssigned();
    _studyLabelModel->setSource(&_availableVariablesModel);
    _studyLabelModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
    ui->studyLabels->setModel(_studyLabelModel);


	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);
	ui->buttonAssignFactors->setSourceAndTarget(ui->listAvailableFields, ui->factors);
	ui->buttonAssignWlsWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);
    ui->buttonAssignStudyLabels->setSourceAndTarget(ui->listAvailableFields, ui->studyLabels);

	_modelModel = new TableModelAnovaModel(this);
	_modelModel->setPiecesCanBeAssigned(false);
	ui->modelTerms->setModel(_modelModel);
	ui->modelTerms->hide();

	connect(_covariatesModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_covariatesModel, SIGNAL(assignmentsChanged()),  this, SLOT(factorsChanged()));
	connect(_covariatesModel, SIGNAL(assignedTo(Terms)), _modelModel, SLOT(addCovariates(Terms)));
	connect(_covariatesModel, SIGNAL(unassigned(Terms)), _modelModel, SLOT(removeVariables(Terms)));

	connect(_factorsModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_factorsModel, SIGNAL(assignmentsChanged()),  this, SLOT(factorsChanged()));
	connect(_factorsModel, SIGNAL(assignedTo(Terms)), _modelModel, SLOT(addFixedFactors(Terms)));
	connect(_factorsModel, SIGNAL(unassigned(Terms)), _modelModel, SLOT(removeVariables(Terms)));

	ui->panelStatistics->hide();
	ui->panelIncludeConstant->hide();
    ui->panelAssumptionChecks->hide();

}

ClassicalMetaAnalysisForm::~ClassicalMetaAnalysisForm()
{
    delete ui;
}

void ClassicalMetaAnalysisForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void ClassicalMetaAnalysisForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}

//ClassicalMetaAnalysisForm::~ClassicalMetaAnalysisForm()
//{
//	delete ui;
//}

void ClassicalMetaAnalysisForm:: bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);
	_modelModel->setVariables(Terms(), Terms(), _covariatesModel->assigned());
}
