#ifndef CORRELATIONBAYESIANFORM_H
#define CORRELATIONBAYESIANFORM_H

#include "analysisform.h"
#include "widgets/tablemodelvariablesassigned.h"

namespace Ui {
class CorrelationBayesianForm;
}

class CorrelationBayesianForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit CorrelationBayesianForm(QWidget *parent = 0);
	~CorrelationBayesianForm();

private:
	Ui::CorrelationBayesianForm *ui;

	TableModelVariablesAssigned *_modelVariables;
};

#endif // CORRELATIONBAYESIANFORM_H
