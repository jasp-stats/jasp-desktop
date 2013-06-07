#ifndef ONEWAYANOVAFORM_H
#define ONEWAYANOVAFORM_H

#include "options.h"
#include "dataset.h"

#include "analysisform.h"

#include "availablefields.h"

namespace Ui {
class ANOVAOneWay;
}

class ANOVAOneWay : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit ANOVAOneWay(QWidget *parent = 0);
	~ANOVAOneWay();

private:
	Ui::ANOVAOneWay *ui;

};

#endif // ONEWAYANOVAFORM_H
