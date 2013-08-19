#ifndef TTESTBAYESONESAMPLEFORM_H
#define TTESTBAYESONESAMPLEFORM_H

#include "analysisform.h"

namespace Ui {
class TTestBayesOneSampleForm;
}

class TTestBayesOneSampleForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestBayesOneSampleForm(QWidget *parent = 0);
	~TTestBayesOneSampleForm();
	
private:
	Ui::TTestBayesOneSampleForm *ui;
};

#endif // TTESTBAYESONESAMPLEFORM_H
