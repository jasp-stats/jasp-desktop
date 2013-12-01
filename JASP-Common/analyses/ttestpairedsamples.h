#ifndef TTESTPAIREDSAMPLES_H
#define TTESTPAIREDSAMPLES_H

#include "analysis.h"

class TTestPairedSamples : public Analysis
{
public:
	TTestPairedSamples(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

#endif // TTESTPAIREDSAMPLES_H
