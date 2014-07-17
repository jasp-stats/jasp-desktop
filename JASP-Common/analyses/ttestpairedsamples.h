#ifndef TTESTPAIREDSAMPLES_H
#define TTESTPAIREDSAMPLES_H

#include "analysis.h"

class TTestPairedSamples : public Analysis
{
public:
	TTestPairedSamples(int id);

protected:

	Options *createOptions() const;

};

#endif // TTESTPAIREDSAMPLES_H
