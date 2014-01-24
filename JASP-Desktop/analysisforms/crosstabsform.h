#ifndef CROSSTABSFORM_H
#define CROSSTABSFORM_H

#include <QWidget>
#include "analysisform.h"

namespace Ui {
class CrosstabsForm;
}

class CrosstabsForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit CrosstabsForm(QWidget *parent = 0);
	~CrosstabsForm();

private:
	Ui::CrosstabsForm *ui;
};

#endif // CROSSTABSFORM_H
