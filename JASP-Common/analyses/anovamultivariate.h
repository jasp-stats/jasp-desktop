#ifndef ANOVAMULTIVARIATE_H
#define ANOVAMULTIVARIATE_H

#include "analysis.h"

class AnovaMultivariate : public Analysis
{
public:
	AnovaMultivariate(int id);

protected:

	Options *createOptions() const;

};

#endif // ANOVAMULTIVARIATE_H
