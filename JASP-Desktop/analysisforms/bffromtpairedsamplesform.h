#ifndef BFFROMTPAIREDSAMPLESFORM_H
#define BFFROMTPAIREDSAMPLESFORM_H

#include "analysisform.h"

namespace Ui {
class BFFromTPairedSamplesForm;
}

class BFFromTPairedSamplesForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit BFFromTPairedSamplesForm(QWidget *parent = 0);
	~BFFromTPairedSamplesForm();

private:
	Ui::BFFromTPairedSamplesForm *ui;
};

#endif // BFFROMTPAIREDSAMPLESFORM_H
