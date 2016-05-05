#ifndef BFFROMTONESAMPLEFORM_H
#define BFFROMTONESAMPLEFORM_H

#include "analysisform.h"

namespace Ui {
class BFFromTOneSampleForm;
}

class BFFromTOneSampleForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit BFFromTOneSampleForm(QWidget *parent = 0);
	~BFFromTOneSampleForm();

private:
	Ui::BFFromTOneSampleForm *ui;
};

#endif // BFFROMTONESAMPLEFORM_H
