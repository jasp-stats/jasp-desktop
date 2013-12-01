#ifndef ANCOVAMULTIVARIATE_H
#define ANCOVAMULTIVARIATE_H

#include "analysis.h"

class AncovaMultivariate : public Analysis
{
public:
	AncovaMultivariate(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

#endif // ANCOVAMULTIVARIATE_H
