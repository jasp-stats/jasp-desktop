#ifndef CORRELATIONPARTIAL_H
#define CORRELATIONPARTIAL_H

#include "analysis.h"

class CorrelationPartial : public Analysis
{
public:
	CorrelationPartial(int id);

protected:
	virtual Options* createDefaultOptions() OVERRIDE;
};

#endif // CORRELATIONPARTIAL_H
