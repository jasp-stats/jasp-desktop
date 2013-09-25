#ifndef ANCOVAFORM_H
#define ANCOVAFORM_H

#include "analysisform.h"

#include "widgets/listmodelvariablesassigned.h"
#include "widgets/listmodelanovamodel.h"

namespace Ui {
class AncovaForm;
}

class AncovaForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AncovaForm(QWidget *parent = 0);
	~AncovaForm();
	
private slots:
	void factorsChanged();
	void dependentChanged();

private:
	Ui::AncovaForm *ui;

	ListModelVariablesAssigned *_dependentListModel;
	ListModelVariablesAssigned *_fixedFactorsListModel;
	ListModelVariablesAssigned *_randomFactorsListModel;
	ListModelVariablesAssigned *_covariatesListModel;
	ListModelVariablesAssigned *_wlsWeightsListModel;

	ListModelAnovaModel *_anovaModel;

	ListModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANCOVAFORM_H
