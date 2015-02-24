#ifndef CORRELATIONBAYESIANPAIRS_H
#define CORRELATIONBAYESIANPAIRS_H

#include "analysis.h"

class CorrelationBayesianPairs : public Analysis
{
public:
	CorrelationBayesianPairs(int id);

protected:

	Options *createOptions() const;

};

#endif // CORRELATIONBAYESIANPAIRS_H
