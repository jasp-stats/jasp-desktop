#ifndef ANOVAREPEATEDMEASURESFORM_H
#define ANOVAREPEATEDMEASURESFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"

namespace Ui {
class AnovaRepeatedMeasuresForm;
}

class AnovaRepeatedMeasuresForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaRepeatedMeasuresForm(QWidget *parent = 0);
	~AnovaRepeatedMeasuresForm();
	
private slots:
	void factorsChanged();
	void termsChanged();

private:
	Ui::AnovaRepeatedMeasuresForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_fixedFactorsListModel;
	TableModelVariablesAssigned *_randomFactorsListModel;
	TableModelVariablesAssigned *_subjectIDsListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesOptions *_contrastsModel;
	TableModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANOVAREPEATEDMEASURESFORM_H
