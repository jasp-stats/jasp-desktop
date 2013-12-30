#ifndef ONEWAYANOVAFORM_H
#define ONEWAYANOVAFORM_H

#include "options/options.h"
#include "dataset.h"

#include "analysisform.h"

#include "availablefields.h"

namespace Ui {
class AnovaOneWayForm;
}

class AnovaOneWayForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaOneWayForm(QWidget *parent = 0);
	~AnovaOneWayForm();

private:
	Ui::AnovaOneWayForm *ui;

};

#endif // ONEWAYANOVAFORM_H
