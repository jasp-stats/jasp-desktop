#ifndef ANCOVAMULTIVARIATEFORM_H
#define ANCOVAMULTIVARIATEFORM_H

#include "analysisform.h"

#include "widgets/listmodelvariablesassigned.h"
#include "widgets/listmodelanovamodel.h"

namespace Ui {
class AncovaMultivariateForm;
}

class AncovaMultivariateForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit AncovaMultivariateForm(QWidget *parent = 0);
	~AncovaMultivariateForm();

private slots:
	void factorsChanged();
	void dependentChanged();

private:
	Ui::AncovaMultivariateForm *ui;

	ListModelVariablesAssigned *_dependentListModel;
	ListModelVariablesAssigned *_fixedFactorsListModel;
	ListModelVariablesAssigned *_covariatesListModel;
	ListModelVariablesAssigned *_wlsWeightsListModel;

	ListModelAnovaModel *_anovaModel;

	ListModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANCOVAMULTIVARIATEFORM_H
