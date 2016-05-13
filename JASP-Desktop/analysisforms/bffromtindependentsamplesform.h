#ifndef BFFROMTINDEPENDENTSAMPLESFORM_H
#define BFFROMTINDEPENDENTSAMPLESFORM_H

#include "analysisform.h"

namespace Ui {
class BFFromTIndependentSamplesForm;
}

class BFFromTIndependentSamplesForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit BFFromTIndependentSamplesForm(QWidget *parent = 0);
	~BFFromTIndependentSamplesForm();

private:
	Ui::BFFromTIndependentSamplesForm *ui;
};

#endif // BFFROMTINDEPENDENTSAMPLESFORM_H
