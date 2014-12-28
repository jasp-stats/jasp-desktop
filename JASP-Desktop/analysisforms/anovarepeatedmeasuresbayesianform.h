#ifndef ANOVAREPEATEDMEASURESBAYESIANFORM_H
#define ANOVAREPEATEDMEASURESBAYESIANFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"
#include "widgets/tablemodelanovadesign.h"
#include "widgets/tablemodelanovawithinsubjectcells.h"

namespace Ui {
class AnovaRepeatedMeasuresBayesianForm;
}

class AnovaRepeatedMeasuresBayesianForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaRepeatedMeasuresBayesianForm(QWidget *parent = 0);
	~AnovaRepeatedMeasuresBayesianForm();
	
private slots:
	void factorsChanged();
	void termsChanged();
	void withinSubjectsDesignChanged();

	void anovaDesignTableClicked(QModelIndex index);

private:
	Ui::AnovaRepeatedMeasuresBayesianForm *ui;

	TableModelAnovaDesign *_designTableModel;
	TableModelAnovaWithinSubjectCells *_withinSubjectCellsListModel;
	TableModelVariablesAssigned *_randomFactorsListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesOptions *_contrastsModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANOVAREPEATEDMEASURESBAYESIANFORM_H
