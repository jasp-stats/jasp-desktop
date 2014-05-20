#ifndef ANOVAMULTIVARIATEFORM_H
#define ANOVAMULTIVARIATEFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

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

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_fixedFactorsListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // ANOVAMULTIVARIATEFORM_H
