#ifndef TTESTBAYESIANPAIREDSAMPLESFORM_H
#define TTESTBAYESIANPAIREDSAMPLESFORM_H

#include "analysisform.h"

namespace Ui {
class TTestBayesianPairedSamplesForm;
}

class TTestBayesianPairedSamplesForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestBayesianPairedSamplesForm(QWidget *parent = 0);
	~TTestBayesianPairedSamplesForm();
	
private:
	Ui::TTestBayesianPairedSamplesForm *ui;
};

#endif // TTESTBAYESIANPAIREDSAMPLESFORM_H
