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

	retain = ui->gPriorParameter->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->gPriorParameter->setSizePolicy(retain);

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
	// hide the g prior parameter, alpha
	ui->label_gPriorParameter->hide();
	ui->gPriorParameter->hide();

	// default behaviour: show beta binomial parameters, hide bernoulli params
	defaultOptionsModelPrior();
}


void BASRegressionLinearLinkForm::defaultOptionsModelPrior()
{
	ui->label_betaBinomialParamA->show();
	ui->label_betaBinomialParamB->show();
	ui->betaBinomialParamA->show();
	ui->betaBinomialParamB->show();
	ui->label_bernoulliParam->hide();
	ui->bernoulliParam->hide();
}


void BASRegressionLinearLinkForm:: bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);
}


void BASRegressionLinearLinkForm::on_MCMC_clicked()
{
	if (!ui->BAS->isChecked() && !ui->MCMC->isChecked())
	{
		ui->MCMC->setChecked(true);
	}

	if (ui->MCMC->isChecked())
	{
		ui->label_iterationsMCMC->show();
		ui->iterationsMCMC->show();
	}
	else
	{
		ui->label_iterationsMCMC->hide();
		ui->iterationsMCMC->hide();
	}
}


void BASRegressionLinearLinkForm::on_BAS_clicked()
{
	if (!ui->BAS->isChecked() && !ui->MCMC->isChecked())
	{
		ui->BAS->setChecked(true);
	}
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


void BASRegressionLinearLinkForm::on_g_prior_clicked()
{
	ui->label_gPriorParameter->show();
	ui->gPriorParameter->show();
}


void BASRegressionLinearLinkForm::on_hyper_g_clicked()
{
	ui->label_gPriorParameter->show();
	ui->gPriorParameter->show();
}


void BASRegressionLinearLinkForm::on_hyper_g_laplace_clicked()
{
	ui->label_gPriorParameter->show();
	ui->gPriorParameter->show();
}


void BASRegressionLinearLinkForm::on_hyper_g_n_clicked()
{
	ui->label_gPriorParameter->show();
	ui->gPriorParameter->show();
}


void BASRegressionLinearLinkForm::on_priorRegressionCoefficients_clicked()
{
	if (ui->g_prior->isChecked())
	{
		ui->label_gPriorParameter->show();
		ui->gPriorParameter->show();
	}
	else
	{
		ui->label_gPriorParameter->hide();
		ui->gPriorParameter->hide();
	}
}
