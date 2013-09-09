#ifndef TTESTBAYESONESAMPLEFORM_H
#define TTESTBAYESONESAMPLEFORM_H

#include "analysisform.h"

namespace Ui {
class TTestBayesianOneSampleForm;
}

class TTestBayesianOneSampleForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestBayesianOneSampleForm(QWidget *parent = 0);
	~TTestBayesianOneSampleForm();
	
private:
	Ui::TTestBayesianOneSampleForm *ui;
};

#endif // TTESTBAYESONESAMPLEFORM_H
