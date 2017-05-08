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

#ifndef BASREGRESSIONLINEARLINKFORM_H
#define BASREGRESSIONLINEARLINKFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

#include <QSizePolicy>
#include <QDebug>


namespace Ui {
class BASRegressionLinearLinkForm;
}

class BASRegressionLinearLinkForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit BASRegressionLinearLinkForm(QWidget *parent = 0);
	~BASRegressionLinearLinkForm();

	void defaultOptions();
	void defaultOptionsModelPrior();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private slots:
	void on_MCMC_clicked();
	void on_BAS_clicked();
	void on_betaBinomial_clicked();
	void on_Bernoulli_clicked();
	void on_uniformPrior_clicked();
	void on_g_prior_clicked();
	void on_hyper_g_clicked();
	void on_hyper_g_laplace_clicked();
	void on_hyper_g_n_clicked();
	void on_priorRegressionCoefficients_clicked();

private:
	Ui::BASRegressionLinearLinkForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_covariatesListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelVariablesAvailable *_factorsAvailableListModel;
};

#endif // BASREGRESSIONLINEARLINKFORM_H
