#ifndef ANOVA_H
#define ANOVA_H

#include "analysisform.h"

namespace Ui {
class ANOVA;
}

class ANOVA : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit ANOVA(QWidget *parent = 0);
	~ANOVA();
	
private:
	Ui::ANOVA *ui;
};

#endif // ANOVA_H
