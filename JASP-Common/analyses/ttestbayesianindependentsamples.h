#ifndef TTESTBAYESIANINDEPENDENTSAMPLES_H
#define TTESTBAYESIANINDEPENDENTSAMPLES_H

#include "analysis.h"

class TTestBayesianIndependentSamples : public Analysis
{
public:
	TTestBayesianIndependentSamples(int id);

protected:

	Options *createOptions() const;

};

#endif // TTESTBAYESIANINDEPENDENTSAMPLES_H
