#ifndef ONESAMPLETTEST_H
#define ONESAMPLETTEST_H

#include "../JASP-Common/analysis.h"

class TTestOneSample : public Analysis
{
public:
	TTestOneSample(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

#endif // ONESAMPLETTEST_H
