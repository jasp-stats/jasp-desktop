#ifndef CORRELATIONBAYESIAN_H
#define CORRELATIONBAYESIAN_H

#include "analysis.h"

class CorrelationBayesian : public Analysis
{
public:
	CorrelationBayesian(int id);

protected:
	Options* createOptions() const;
};

#endif // CORRELATIONBAYESIAN_H
