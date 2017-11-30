//
// Copyright (C) 2017 University of Amsterdam
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

#include "basregressionlinearlinkform.h"
#include "ui_basregressionlinearlinkform.h"


BASRegressionLinearLinkForm::BASRegressionLinearLinkForm(QWidget *parent) :
	AnalysisForm("BASRegressionLinearLinkForm", parent),
	ui(new Ui::BASRegressionLinearLinkForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_dependentListModel->setSource(&_availableVariablesModel);
	ui->dependent->setModel(_dependentListModel);

	_covariatesListModel = new TableModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableVariablesModel);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->covariates->setModel(_covariatesListModel);

	_wlsWeightsListModel = new TableModelVariablesAssigned();
	_wlsWeightsListModel->setSource(&_availableVariablesModel);
	_wlsWeightsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_wlsWeightsListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->wlsWeights->setModel(_wlsWeightsListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);
	ui->buttonAssignWlsWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);

	ui->plotOptions->hide();
	ui->advancedOptions->hide();

	_anovaModel = new TableModelAnovaModel(this);
	_anovaModel->setPiecesCanBeAssigned(false);
	ui->modelTerms->setModel(_anovaModel);
	ui->modelTerms->hide();

	connect(_covariatesListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_covariatesListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addCovariates(Terms)));
	connect(_covariatesListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	// retain widget sizes when hidden
	QSizePolicy retain = ui->iterationsMCMC->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->iterationsMCMC->setSizePolicy(retain);

	retain = ui->betaBinomialParamA->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->betaBinomialParamA->setSizePolicy(retain);

	retain = ui->betaBinomialParamB->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->betaBinomialParamB->setSizePolicy(retain);

	retain = ui->bernoulliParam->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->bernoulliParam->setSizePolicy(retain);

	retain = ui->alpha->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->alpha->setSizePolicy(retain);

	retain = ui->rScale->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->rScale->setSizePolicy(retain);

	defaultOptions();
}


BASRegressionLinearLinkForm::~BASRegressionLinearLinkForm()
{
	delete ui;
}


void BASRegressionLinearLinkForm::defaultOptions()
{
	// default behaviour: hide the number of iterations for MCMC
	ui->label_iterationsMCMC->hide();
	ui->iterationsMCMC->hide();
	// hide the alpha parameter
	ui->label_alpha->hide();
	ui->alpha->hide();

	// default behaviour: show beta binomial parameters, hide bernoulli params
	defaultOptionsModelPrior();
}


void BASRegressionLinearLinkForm::defaultOptionsModelPrior()
{
	ui->label_betaBinomialParamA->hide();
	ui->label_betaBinomialParamB->hide();
	ui->betaBinomialParamA->hide();
	ui->betaBinomialParamB->hide();
	ui->label_bernoulliParam->hide();
	ui->bernoulliParam->hide();
}


void BASRegressionLinearLinkForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	_anovaModel->setVariables(Terms(), Terms(), _covariatesListModel->assigned());

	factorsChanged();
}


void BASRegressionLinearLinkForm::on_BAS_clicked()
{
	ui->label_numberOfModels->show();
	ui->numberOfModels->show();
	ui->label_iterationsMCMC->hide();
	ui->iterationsMCMC->hide();
}


void BASRegressionLinearLinkForm::on_MCMC_clicked()
{
	ui->label_numberOfModels->hide();
	ui->numberOfModels->hide();
	ui->label_iterationsMCMC->show();
	ui->iterationsMCMC->show();
}


void BASRegressionLinearLinkForm::on_betaBinomial_clicked()
{
	if (ui->betaBinomial->isChecked())
	{
		defaultOptionsModelPrior();
	}
}


void BASRegressionLinearLinkForm::on_Bernoulli_clicked()
{
	if (ui->Bernoulli->isChecked())
	{
		ui->label_betaBinomialParamA->hide();
		ui->label_betaBinomialParamB->hide();
		ui->betaBinomialParamA->hide();
		ui->betaBinomialParamB->hide();
		ui->label_bernoulliParam->show();
		ui->bernoulliParam->show();
	}
}


void BASRegressionLinearLinkForm::on_uniformPrior_clicked()
{
	if (ui->uniformPrior->isChecked())
	{
		ui->label_betaBinomialParamA->hide();
		ui->label_betaBinomialParamB->hide();
		ui->betaBinomialParamA->hide();
		ui->betaBinomialParamB->hide();
		ui->label_bernoulliParam->hide();
		ui->bernoulliParam->hide();
	}
}

void BASRegressionLinearLinkForm::defaultPriorOpts() {
	ui->label_alpha->hide();
	ui->alpha->hide();
	ui->label_rScale->hide();
	ui->rScale->hide();
}

void BASRegressionLinearLinkForm::on_g_prior_clicked()
{
	defaultPriorOpts();
}

void BASRegressionLinearLinkForm::on_jzs_clicked()
{
	ui->label_alpha->hide();
	ui->alpha->hide();
	ui->label_rScale->show();
	ui->rScale->show();
}


void BASRegressionLinearLinkForm::on_hyper_g_clicked()
{
	ui->label_alpha->show();
	ui->alpha->show();
	ui->label_rScale->hide();
	ui->rScale->hide();
}


void BASRegressionLinearLinkForm::on_hyper_g_laplace_clicked()
{
	ui->label_alpha->show();
	ui->alpha->show();
	ui->label_rScale->hide();
	ui->rScale->hide();
}


void BASRegressionLinearLinkForm::on_hyper_g_n_clicked()
{
	ui->label_alpha->show();
	ui->alpha->show();
	ui->label_rScale->hide();
	ui->rScale->hide();
}


void BASRegressionLinearLinkForm::on_aic_clicked()
{
	defaultPriorOpts();
}


void BASRegressionLinearLinkForm::on_bic_clicked()
{
	defaultPriorOpts();
}


void BASRegressionLinearLinkForm::on_eb_global_clicked()
{
	defaultPriorOpts();
}


void BASRegressionLinearLinkForm::on_eb_local_clicked()
{
	defaultPriorOpts();
}


void BASRegressionLinearLinkForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}


void BASRegressionLinearLinkForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}
