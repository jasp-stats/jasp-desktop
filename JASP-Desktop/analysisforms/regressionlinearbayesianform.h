#ifndef REGRESSIONLINEARBAYESIANFORM_H
#define REGRESSIONLINEARBAYESIANFORM_H

#include "analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"

namespace Ui {
class RegressionLinearBayesianForm;
}

class RegressionLinearBayesianForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit RegressionLinearBayesianForm(QWidget *parent = 0);
	~RegressionLinearBayesianForm();

private slots:
	void factorsChanged();
	
private:
	Ui::RegressionLinearBayesianForm *ui;

	TableModelVariablesAssigned *_dependentListModel;
	TableModelVariablesAssigned *_covariatesListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesAvailable *_factorsAvailableListModel;
};

#endif // REGRESSIONLINEARBAYESIANFORM_H
