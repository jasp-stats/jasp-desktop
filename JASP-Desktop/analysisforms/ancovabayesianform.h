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

#ifndef ANCOVABAYESIANFORM_H
#define ANCOVABAYESIANFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class AncovaBayesianForm;
}

class AncovaBayesianForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AncovaBayesianForm(QWidget *parent = 0);
	~AncovaBayesianForm();

	void bindTo(Options *options, DataSet *dataSet) OVERRIDE;
	
private:
	Ui::AncovaBayesianForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_fixedFactorsListModel;
	TableModelVariablesAssigned *_randomFactorsListModel;
	TableModelVariablesAssigned *_covariatesListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesAvailable *_factorsAvailableListModel;

	TableModelVariablesAvailable *_plotFactorsAvailableTableModel;
  TableModelVariablesAssigned *_horizontalAxisTableModel;
  TableModelVariablesAssigned *_seperateLinesTableModel;
  TableModelVariablesAssigned *_seperatePlotsTableModel;
	
private slots:
	void factorsChanging();
	void factorsChanged();
};

#endif // ANCOVABAYESIANFORM_H
