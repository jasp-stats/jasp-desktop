#ifndef ANALYSISFORMTTESTINDEPENDENTSAMPLES_H
#define ANALYSISFORMTTESTINDEPENDENTSAMPLES_H

#include <QWidget>
#include "analysisform.h"

namespace Ui {
class TTestIndependentSamples;
}

class TTestIndependentSamples : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestIndependentSamples(QWidget *parent = 0);
	~TTestIndependentSamples();
	
private:
	Ui::TTestIndependentSamples *ui;
};

#endif // ANALYSISFORMTTESTINDEPENDENTSAMPLES_H
