#ifndef ANOVAFORM_H
#define ANOVAFORM_H

#include "analysisform.h"

#include "widgets/listmodelvariablesassigned.h"
#include "widgets/listmodelanovamodel.h"

namespace Ui {
class AnovaForm;
}

class AnovaForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaForm(QWidget *parent = 0);
	~AnovaForm();
	
private slots:
	void factorsChanged();
	void dependentChanged();

private:
	Ui::AnovaForm *ui;

	ListModelVariablesAssigned *_dependentListModel;
	ListModelVariablesAssigned *_fixedFactorsListModel;
	ListModelVariablesAssigned *_randomFactorsListModel;
	ListModelVariablesAssigned *_wlsWeightsListModel;

	ListModelAnovaModel *_anovaModel;

	ListModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANOVAFORM_H
