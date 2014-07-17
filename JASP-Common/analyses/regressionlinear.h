#ifndef REGRESSIONLINEAR_H
#define REGRESSIONLINEAR_H

#include "analysis.h"

class RegressionLinear : public Analysis
{
public:
	RegressionLinear(int id);

protected:
	Options *createOptions() const;
};

#endif // REGRESSIONLINEAR_H
