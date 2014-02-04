#ifndef SEMSIMPLEFORM_H
#define SEMSIMPLEFORM_H

#include "analysisform.h"

namespace Ui {
class SEMSimpleForm;
}

class SEMSimpleForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit SEMSimpleForm(QWidget *parent = 0);
	~SEMSimpleForm();

private:
	Ui::SEMSimpleForm *ui;
};

#endif // SEMSIMPLEFORM_H
