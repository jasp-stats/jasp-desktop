#ifndef TTESTBAYESIANONESAMPLE_H
#define TTESTBAYESIANONESAMPLE_H

#include "analysis.h"

class TTestBayesianOneSample : public Analysis
{
public:
	TTestBayesianOneSample(int id);
protected:
	virtual Options *createDefaultOptions() OVERRIDE;
};

#endif // TTESTBAYESIANONESAMPLE_H
