#ifndef ONESAMPLETTEST_H
#define ONESAMPLETTEST_H

#include "../JASP-Common/analysis.h"

class TTestOneSample : public Analysis
{
public:
	TTestOneSample(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;
	virtual std::string order() OVERRIDE;

};

#endif // ONESAMPLETTEST_H
