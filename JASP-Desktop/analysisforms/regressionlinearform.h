#ifndef REGRESSIONLINEARFORM_H
#define REGRESSIONLINEARFORM_H

#include "analysisform.h"
#include "widgets/tablemodelvariablesassigned.h"
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

	TableModelVariablesAssigned *_dependentModel;
	TableModelVariablesLevels  *_blocksModel;
	TableModelVariablesAssigned *_wlsWeightsModel;
};

#endif // REGRESSIONLINEARFORM_H
