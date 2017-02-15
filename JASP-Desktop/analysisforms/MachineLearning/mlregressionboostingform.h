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

#ifndef MLREGRESSIONBOOSTINGFORM_H
#define MLREGRESSIONBOOSTINGFORM_H

#include <QSizePolicy>

#include "../analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class MLRegressionBoostingForm;
}


class MLRegressionBoostingForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit MLRegressionBoostingForm(QWidget *parent = 0);
	~MLRegressionBoostingForm();
	void bindTo(Options *options, DataSet *dataSet) OVERRIDE;
	void defaultOptions();

private:
	Ui::MLRegressionBoostingForm *ui;

	TableModelVariablesAssigned *_targetListModel;
	TableModelVariablesAssigned *_predictorsListModel;
	TableModelVariablesAssigned *_indicatorListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

private slots:
	void factorsChanging();
	void factorsChanged();

    void on_plotPredictivePerformance_clicked(bool checked);
};

#endif // MLREGRESSIONBOOSTINGFORM_H
