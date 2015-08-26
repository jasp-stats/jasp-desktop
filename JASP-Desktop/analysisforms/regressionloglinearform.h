#ifndef REGRESSIONLOGLINEARFORM_H
#define REGRESSIONLOGLINEARFORM_H

#include "analysisform.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelvariableslevels.h"
#include "widgets/tablemodelanovamodel.h"

#include "common.h"

namespace Ui {
class RegressionLogLinearForm;
}

class RegressionLogLinearForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit RegressionLogLinearForm(QWidget *parent = 0);
	~RegressionLogLinearForm();
	void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private:
	Ui::RegressionLogLinearForm *ui;

	TableModelVariablesAssigned *_countsModel;
	TableModelVariablesAssigned  *_factorsModel;

	TableModelAnovaModel *_model;
};

#endif // REGRESSIONLOGLINEARFORM_H
