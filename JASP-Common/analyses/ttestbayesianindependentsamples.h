#ifndef TTESTBAYESIANINDEPENDENTSAMPLES_H
#define TTESTBAYESIANINDEPENDENTSAMPLES_H

#include "analysis.h"

class TTestBayesianIndependentSamples : public Analysis
{
public:
	TTestBayesianIndependentSamples(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;
	virtual std::string order() OVERRIDE;

};

#endif // TTESTBAYESIANINDEPENDENTSAMPLES_H
