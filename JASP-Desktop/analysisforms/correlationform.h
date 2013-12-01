#ifndef CORRELATIONFORM_H
#define CORRELATIONFORM_H

#include "analysisform.h"

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
};

#endif // CORRELATIONFORM_H
