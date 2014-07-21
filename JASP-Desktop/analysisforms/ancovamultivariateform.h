#ifndef ANCOVAMULTIVARIATEFORM_H
#define ANCOVAMULTIVARIATEFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

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

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_fixedFactorsListModel;
	TableModelVariablesAssigned *_covariatesListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANCOVAMULTIVARIATEFORM_H
