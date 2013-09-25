#ifndef ANOVAMULTIVARIATEFORM_H
#define ANOVAMULTIVARIATEFORM_H

#include "analysisform.h"

#include "widgets/listmodelvariablesassigned.h"
#include "widgets/listmodelanovamodel.h"

namespace Ui {
class AnovaMultivariateForm;
}

class AnovaMultivariateForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit AnovaMultivariateForm(QWidget *parent = 0);
	~AnovaMultivariateForm();

private slots:
	void factorsChanged();
	void dependentChanged();

private:
	Ui::AnovaMultivariateForm *ui;

	ListModelVariablesAssigned *_dependentListModel;
	ListModelVariablesAssigned *_fixedFactorsListModel;
	ListModelVariablesAssigned *_wlsWeightsListModel;

	ListModelAnovaModel *_anovaModel;

	ListModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANOVAMULTIVARIATEFORM_H
