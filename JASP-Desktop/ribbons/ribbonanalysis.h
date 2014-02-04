#ifndef RIBBONANALYSIS_H
#define RIBBONANALYSIS_H

#include "ribbonwidget.h"

namespace Ui {
class RibbonAnalysis;
}

class RibbonAnalysis : public RibbonWidget
{
	Q_OBJECT
	
public:
	explicit RibbonAnalysis(QWidget *parent = 0);
	~RibbonAnalysis();

private:
	Ui::RibbonAnalysis *ui;
};

#endif // RIBBONANALYSIS_H
