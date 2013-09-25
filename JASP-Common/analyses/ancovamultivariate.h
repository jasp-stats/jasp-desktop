#ifndef ANCOVAMULTIVARIATE_H
#define ANCOVAMULTIVARIATE_H

#include "../JASP-Common/analysis.h"

namespace analyses
{

class AncovaMultivariate : public Analysis
{
public:
	AncovaMultivariate(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

}

#endif // ANCOVAMULTIVARIATE_H
