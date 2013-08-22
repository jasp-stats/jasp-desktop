#ifndef ANOVAMULTIVARIATE_H
#define ANOVAMULTIVARIATE_H

#include "../JASP-Common/analysis.h"

namespace analyses
{

class AnovaMultivariate : public Analysis
{
public:
	AnovaMultivariate(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

}

#endif // ANOVAMULTIVARIATE_H
