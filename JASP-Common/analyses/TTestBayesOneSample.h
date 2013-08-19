#ifndef TTESTBAYESONESAMPLE_H
#define TTESTBAYESONESAMPLE_H

#endif // TTESTBAYESONESAMPLE_H

#include "analysis.h"

namespace analyses
{

class TTestBayesOneSample : public Analysis
{
public:
	TTestBayesOneSample(int id);
protected:
	virtual Options *createDefaultOptions() OVERRIDE;
};

}
