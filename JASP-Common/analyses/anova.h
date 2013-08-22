#ifndef ANOVA_H
#define ANOVA_H

#include "../JASP-Common/analysis.h"

namespace analyses
{

class Anova : public Analysis
{
public:
	Anova(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

}

#endif // ANOVA_H
