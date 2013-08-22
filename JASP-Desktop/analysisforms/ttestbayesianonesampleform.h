#ifndef TTESTBAYESONESAMPLEFORM_H
#define TTESTBAYESONESAMPLEFORM_H

#include "analysisform.h"

namespace Ui {
class TTestBayesOneSampleForm;
}

class TTestBayesianOneSampleForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestBayesianOneSampleForm(QWidget *parent = 0);
	~TTestBayesianOneSampleForm();
	
private:
	Ui::TTestBayesOneSampleForm *ui;
};

#endif // TTESTBAYESONESAMPLEFORM_H
