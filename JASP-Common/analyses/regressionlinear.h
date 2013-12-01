#ifndef REGRESSIONLINEAR_H
#define REGRESSIONLINEAR_H

#include "analysis.h"

class RegressionLinear : public Analysis
{
public:
	RegressionLinear(int id);

protected:
	virtual Options *createDefaultOptions() OVERRIDE;
};

#endif // REGRESSIONLINEAR_H
