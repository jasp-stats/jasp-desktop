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

#ifndef REGRESSIONLINEARFORM_H
#define REGRESSIONLINEARFORM_H

#include "analysisform.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelvariableslevels.h"
#include "widgets/tablemodelanovamodel.h"

#include "common.h"

namespace Ui {
class RegressionLinearForm;
}

class RegressionLinearForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit RegressionLinearForm(QWidget *parent = 0);
	~RegressionLinearForm();
	void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private slots:
	void factorsChanging();
	void factorsChanged();

private:
	Ui::RegressionLinearForm *ui;

	TableModelVariablesAssigned *_dependentModel;
	TableModelVariablesAssigned  *_covariatesModel;
	TableModelVariablesAssigned  *_factorsModel;
	TableModelVariablesAssigned *_wlsWeightsModel;

	TableModelAnovaModel *_modelModel;
};

#endif // REGRESSIONLINEARFORM_H
