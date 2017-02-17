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

#ifndef MLREGRESSIONKNNFORM_H
#define MLREGRESSIONKNNFORM_H

#include <QSizePolicy>

#include "../analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class MLRegressionKNNForm;
}


class MLRegressionKNNForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit MLRegressionKNNForm(QWidget *parent = 0);
	~MLRegressionKNNForm();
	void bindTo(Options *options, DataSet *dataSet) OVERRIDE;
	void defaultOptions();

private:
	Ui::MLRegressionKNNForm *ui;

	TableModelVariablesAssigned *_targetListModel;
	TableModelVariablesAssigned *_predictorsListModel;
	TableModelVariablesAssigned *_indicatorListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;
	TableModelAnovaModel *_anovaModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

private slots:
	void factorsChanging();
	void factorsChanged();
	void on_manual_1_clicked(bool);
	void on_auto_1_clicked(bool);
	void on_manual_2_clicked(bool);
	void on_auto_2_clicked(bool);
	void on_manual_3_clicked(bool);
	void on_auto_3_clicked(bool);
	void on_manual_4_clicked(bool);
	void on_auto_4_clicked(bool);
	void on_manual_5_clicked(bool);
	void on_auto_5_clicked(bool);
	void on_manual_6_clicked(bool);
	void on_auto_6_clicked(bool);
	void on_manual_7_clicked(bool);
	void on_auto_7_clicked(bool);
};

#endif // MLREGRESSIONKNNFORM_H
