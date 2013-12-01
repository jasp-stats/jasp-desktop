#ifndef REGRESSIONLINEARFORM_H
#define REGRESSIONLINEARFORM_H

#include "analysisform.h"

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
};

#endif // REGRESSIONLINEARFORM_H
