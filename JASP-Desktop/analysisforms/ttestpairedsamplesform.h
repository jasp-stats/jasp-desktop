#ifndef TTESTPAIREDSAMPLESFORM_H
#define TTESTPAIREDSAMPLESFORM_H

#include "analysisform.h"

namespace Ui {
class TTestPairedSamplesForm;
}

class TTestPairedSamplesForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestPairedSamplesForm(QWidget *parent = 0);
	~TTestPairedSamplesForm();
	
private:
	Ui::TTestPairedSamplesForm *ui;
};

#endif // TTESTPAIREDSAMPLESFORM_H
