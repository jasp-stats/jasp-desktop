#ifndef ONEWAYANOVAFORM_H
#define ONEWAYANOVAFORM_H

#include "options.h"
#include "dataset.h"

#include "analysisform.h"

#include "availablefields.h"

namespace Ui {
class ANOVAOneWay;
}

class AnovaOneWay : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaOneWay(QWidget *parent = 0);
	~AnovaOneWay();

private:
	Ui::ANOVAOneWay *ui;

};

#endif // ONEWAYANOVAFORM_H
