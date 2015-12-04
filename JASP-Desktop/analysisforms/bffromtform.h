#ifndef BFFROMTFORM_H
#define BFFROMTFORM_H

#include "analysisform.h"

namespace Ui {
class BFFromTForm;
}

class BFFromTForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit BFFromTForm(QWidget *parent = 0);
	~BFFromTForm();

private:
	Ui::BFFromTForm *ui;
};

#endif // BFFROMTFORM_H
