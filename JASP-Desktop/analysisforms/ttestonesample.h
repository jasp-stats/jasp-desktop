#ifndef FORMANALYSISONESAMPLETTEST_H
#define FORMANALYSISONESAMPLETTEST_H

#include <QWidget>
#include "analysisform.h"

namespace Ui {
class TTestOneSample;
}

class TTestOneSample : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestOneSample(QWidget *parent = 0);
	~TTestOneSample();
	
private:
	Ui::TTestOneSample *ui;
};

#endif // FORMANALYSISONESAMPLETTEST_H
