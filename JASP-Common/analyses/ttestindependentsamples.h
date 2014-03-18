#ifndef TTESTINDEPENDENTSAMPLES_H
#define TTESTINDEPENDENTSAMPLES_H

#include "analysis.h"

class TTestIndependentSamples : public Analysis
{
public:
	TTestIndependentSamples(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;
	virtual std::string order() OVERRIDE;

};

#endif // TTESTINDEPENDENTSAMPLES_H
