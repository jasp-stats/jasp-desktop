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

#ifndef REGRESSIONLOGLINEARBAYESIANFORM_H
#define REGRESSIONLOGLINEARBAYESIANFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class RegressionLogLinearBayesianForm;
}

class RegressionLogLinearBayesianForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit RegressionLogLinearBayesianForm(QWidget *parent = 0);
	~RegressionLogLinearBayesianForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private slots:
	void factorsChanging();
	void factorsChanged();
	
private:
	Ui::RegressionLogLinearBayesianForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_factorsListModel;

	TableModelAnovaModel *_model;

	TableModelVariablesAvailable *_factorsAvailableListModel;
};

#endif // REGRESSIONLOGLINEARBAYESIANFORM_H
