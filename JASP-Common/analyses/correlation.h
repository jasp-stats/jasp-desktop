#ifndef CORRELATION_H
#define CORRELATION_H

#include "analysis.h"

class Correlation : public Analysis
{
public:
	Correlation(int id);

protected:
	virtual Options* createDefaultOptions() OVERRIDE;
};

#endif // CORRELATION_H
