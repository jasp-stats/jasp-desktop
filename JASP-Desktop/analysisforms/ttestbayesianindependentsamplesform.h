#ifndef TTESTBAYESIANINDEPENDENTSAMPLESFORM_H
#define TTESTBAYESIANINDEPENDENTSAMPLESFORM_H

#include <QWidget>
#include "analysisform.h"

namespace Ui {
class TTestBayesianIndependentSamplesForm;
}

class TTestBayesianIndependentSamplesForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestBayesianIndependentSamplesForm(QWidget *parent = 0);
	~TTestBayesianIndependentSamplesForm();
	
private:
	Ui::TTestBayesianIndependentSamplesForm *ui;
};

#endif // TTESTBAYESIANINDEPENDENTSAMPLESFORM_H
