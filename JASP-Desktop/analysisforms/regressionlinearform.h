#ifndef REGRESSIONLINEARFORM_H
#define REGRESSIONLINEARFORM_H

#include "analysisform.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelvariableslevels.h"
#include "widgets/tablemodelanovamodel.h"

#include "common.h"

namespace Ui {
class RegressionLinearForm;
}

class RegressionLinearForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit RegressionLinearForm(QWidget *parent = 0);
	~RegressionLinearForm();
	void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private:
	Ui::RegressionLinearForm *ui;

	TableModelVariablesAssigned *_dependentModel;
	TableModelVariablesAssigned  *_covariatesModel;
	TableModelVariablesAssigned *_wlsWeightsModel;

	TableModelAnovaModel *_modelModel;
};

#endif // REGRESSIONLINEARFORM_H
