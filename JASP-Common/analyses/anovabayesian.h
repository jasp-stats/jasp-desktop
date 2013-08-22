#ifndef ANOVABAYESIAN_H
#define ANOVABAYESIAN_H

#include "../JASP-Common/analysis.h"

namespace analyses
{

class AnovaBayesian : public Analysis
{
public:
	AnovaBayesian(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

}

#endif // ANOVABAYESIAN_H
