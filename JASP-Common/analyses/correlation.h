#ifndef CORRELATION_H
#define CORRELATION_H

#include "analysis.h"

class Correlation : public Analysis
{
public:
	Correlation(int id);

protected:
	Options* createOptions() const;
};

#endif // CORRELATION_H
