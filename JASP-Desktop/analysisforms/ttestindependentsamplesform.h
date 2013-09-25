#ifndef TTESTINDEPENDENTSAMPLESFORM_H
#define TTESTINDEPENDENTSAMPLESFORM_H

#include <QWidget>
#include "analysisform.h"

namespace Ui {
class TTestIndependentSamplesForm;
}

class TTestIndependentSamplesForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestIndependentSamplesForm(QWidget *parent = 0);
	~TTestIndependentSamplesForm();
	
private:
	Ui::TTestIndependentSamplesForm *ui;
};

#endif // TTESTINDEPENDENTSAMPLESFORM_H
