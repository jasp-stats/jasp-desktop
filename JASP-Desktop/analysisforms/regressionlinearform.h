#ifndef REGRESSIONLINEARFORM_H
#define REGRESSIONLINEARFORM_H

#include "analysisform.h"
#include "widgets/listmodelvariablesassigned.h"
#include "widgets/tablemodelvariableslevels.h"

namespace Ui {
class RegressionLinearForm;
}

class RegressionLinearForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit RegressionLinearForm(QWidget *parent = 0);
	~RegressionLinearForm();

private:
	Ui::RegressionLinearForm *ui;

	ListModelVariablesAssigned *_dependentModel;
	TableModelVariablesLevels  *_blocksModel;
	ListModelVariablesAssigned *_wlsWeightsModel;
};

#endif // REGRESSIONLINEARFORM_H
