#ifndef ANCOVAMULTIVARIATE_H
#define ANCOVAMULTIVARIATE_H

#include "analysis.h"

class AncovaMultivariate : public Analysis
{
public:
	AncovaMultivariate(int id);

protected:

	Options *createOptions() const;

};

#endif // ANCOVAMULTIVARIATE_H
