#ifndef TTESTBAYESIANPAIREDSAMPLES_H
#define TTESTBAYESIANPAIREDSAMPLES_H

#include "analysis.h"

class TTestBayesianPairedSamples : public Analysis
{
public:
	TTestBayesianPairedSamples(int id);

protected:

	Options *createOptions() const;

};

#endif // TTESTBAYESIANPAIREDSAMPLES_H
