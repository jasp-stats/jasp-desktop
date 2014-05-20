#ifndef CORRELATIONFORM_H
#define CORRELATIONFORM_H

#include "analysisform.h"
#include "widgets/tablemodelvariablesassigned.h"

namespace Ui {
class CorrelationForm;
}

class CorrelationForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit CorrelationForm(QWidget *parent = 0);
	~CorrelationForm();

private:
	Ui::CorrelationForm *ui;

	TableModelVariablesAssigned *_modelVariables;
};

#endif // CORRELATIONFORM_H
