#ifndef ANCOVA_H
#define ANCOVA_H

#include "../JASP-Common/analysis.h"

namespace analyses
{

class Ancova : public Analysis
{
public:
	Ancova(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

}

#endif // ANCOVA_H
