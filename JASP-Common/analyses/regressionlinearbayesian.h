#ifndef REGRESSIONLINEARBAYESIAN_H
#define REGRESSIONLINEARBAYESIAN_H

#include "analysis.h"

class RegressionLinearBayesian : public Analysis
{
public:
	RegressionLinearBayesian(int id);

protected:

	Options *createOptions() const;

};

#endif // REGRESSIONLINEARBAYESIAN_H
