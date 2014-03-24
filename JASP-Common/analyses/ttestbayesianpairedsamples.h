#ifndef TTESTBAYESIANPAIREDSAMPLES_H
#define TTESTBAYESIANPAIREDSAMPLES_H

#include "analysis.h"

class TTestBayesianPairedSamples : public Analysis
{
public:
	TTestBayesianPairedSamples(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;
	virtual std::string order() OVERRIDE;

};

#endif // TTESTBAYESIANPAIREDSAMPLES_H
