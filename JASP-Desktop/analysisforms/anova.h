#ifndef ANOVA_H
#define ANOVA_H

#include "analysisform.h"

namespace Ui {
class ANOVA;
}

class Anova : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit Anova(QWidget *parent = 0);
	~Anova();
	
private:
	Ui::ANOVA *ui;
};

#endif // ANOVA_H
