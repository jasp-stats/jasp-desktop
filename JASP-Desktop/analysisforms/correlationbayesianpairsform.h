#ifndef CORRELATIONBAYESIANPAIRSFORM_H
#define CORRELATIONBAYESIANPAIRSFORM_H

#include "analysisform.h"

namespace Ui {
class CorrelationBayesianPairsForm;
}

class CorrelationBayesianPairsForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit CorrelationBayesianPairsForm(QWidget *parent = 0);
	~CorrelationBayesianPairsForm();
	
private:
	Ui::CorrelationBayesianPairsForm *ui;
};

#endif // CORRELATIONBAYESIANPAIRSFORM_H
