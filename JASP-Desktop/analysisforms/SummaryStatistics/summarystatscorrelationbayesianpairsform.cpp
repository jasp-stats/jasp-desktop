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

#include "summarystatscorrelationbayesianpairsform.h"
#include "ui_summarystatscorrelationbayesianpairsform.h"


SummaryStatsCorrelationBayesianPairsForm::SummaryStatsCorrelationBayesianPairsForm(QWidget *parent) :
	AnalysisForm("SummaryStatsCorrelationBayesianPairsForm", parent),
	ui(new Ui::SummaryStatsCorrelationBayesianPairsForm)
{
	ui->setupUi(this);
	QSizePolicy retainKendallTau = ui->kendallTauValue->sizePolicy();
	retainKendallTau.setRetainSizeWhenHidden(true);
	ui->kendallTauValue->setSizePolicy(retainKendallTau);

	QSizePolicy retainPearsonRho = ui->pearsonRhoValue->sizePolicy();
	retainPearsonRho.setRetainSizeWhenHidden(true);
	ui->pearsonRhoValue->setSizePolicy(retainPearsonRho);

	ui->pearsonRho->setChecked(true);
	ui->kendallTauValue->hide();
}

SummaryStatsCorrelationBayesianPairsForm::~SummaryStatsCorrelationBayesianPairsForm()
{
	delete ui;
}

void SummaryStatsCorrelationBayesianPairsForm::on_pearsonRho_clicked(bool checked)
{
    if (checked)
    {
        ui->pearsonRhoValue->show();
        ui->kendallTauValue->hide();
    }
}

void SummaryStatsCorrelationBayesianPairsForm::on_kendallTau_clicked(bool checked)
{
    if (checked)
    {
        ui->pearsonRhoValue->hide();
        ui->kendallTauValue->show();
    }
}
