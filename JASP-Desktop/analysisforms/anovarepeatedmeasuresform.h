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

#ifndef ANOVAREPEATEDMEASURESFORM_H
#define ANOVAREPEATEDMEASURESFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"
#include "widgets/tablemodelanovadesign.h"
#include "widgets/tablemodelanovawithinsubjectcells.h"

namespace Ui {
class AnovaRepeatedMeasuresForm;
}

class AnovaRepeatedMeasuresForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaRepeatedMeasuresForm(QWidget *parent = 0);
	~AnovaRepeatedMeasuresForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;
	
private slots:
	void factorsChanging();
	void factorsChanged();
	void termsChanged();
	void withinSubjectsDesignChanged();

	void anovaDesignTableClicked(QModelIndex index);

private:
	Ui::AnovaRepeatedMeasuresForm *ui;

	TableModelAnovaDesign *_designTableModel;
	TableModelAnovaWithinSubjectCells *_withinSubjectCellsListModel;
	TableModelVariablesAssigned *_betweenSubjectsFactorsListModel;
	TableModelVariablesAssigned *_covariatesListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_withinSubjectsTermsModel;
	TableModelAnovaModel *_betweenSubjectsTermsModel;

	TableModelVariablesOptions *_contrastsModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

	TableModelVariablesAvailable *_plotFactorsAvailableTableModel;
	TableModelVariablesAssigned *_horizontalAxisTableModel;
	TableModelVariablesAssigned *_seperateLinesTableModel;
	TableModelVariablesAssigned *_seperatePlotsTableModel;

};

#endif // ANOVAREPEATEDMEASURESFORM_H
