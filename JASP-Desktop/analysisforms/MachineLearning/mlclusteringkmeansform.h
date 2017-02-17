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

#ifndef MLCLUSTERINGKMEANSFORM_H
#define MLCLUSTERINGKMEANSFORM_H

#include <QSizePolicy>

#include "../analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class MLClusteringKMeansForm;
}


class MLClusteringKMeansForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit MLClusteringKMeansForm(QWidget *parent = 0);
	~MLClusteringKMeansForm();
	void bindTo(Options *options, DataSet *dataSet) OVERRIDE;
	void defaultOptions();

private:
	Ui::MLClusteringKMeansForm *ui;

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
	void on_optimized_2_clicked(bool);
	void on_manual_3_clicked(bool);
	void on_auto_3_clicked(bool);
	void on_optimized_3_clicked(bool);
};

#endif // MLCLUSTERINGKMEANSFORM_H
