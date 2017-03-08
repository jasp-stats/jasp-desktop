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

#ifndef REGRESSIONLINEARBAYESIANFORM_H
#define REGRESSIONLINEARBAYESIANFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class RegressionLinearBayesianForm;
}

class RegressionLinearBayesianForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit RegressionLinearBayesianForm(QWidget *parent = 0);
	~RegressionLinearBayesianForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private slots:
	void factorsChanging();
	void factorsChanged();
	
private:
	Ui::RegressionLinearBayesianForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_covariatesListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesAvailable *_factorsAvailableListModel;
};

#endif // REGRESSIONLINEARBAYESIANFORM_H
