#ifndef CORRELATIONPARTIALFORM_H
#define CORRELATIONPARTIALFORM_H

#include <QWidget>
#include "analysisform.h"

namespace Ui {
class CorrelationPartialForm;
}

class CorrelationPartialForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit CorrelationPartialForm(QWidget *parent = 0);
	~CorrelationPartialForm();

private:
	Ui::CorrelationPartialForm *ui;

	TableModelVariablesAssigned *_modelVariables;
	TableModelVariablesAssigned *_modelControllingFor;
};

#endif // CORRELATIONPARTIALFORM_H
