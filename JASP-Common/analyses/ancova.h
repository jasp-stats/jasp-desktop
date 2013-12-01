#ifndef ANCOVA_H
#define ANCOVA_H

#include "analysis.h"

class Ancova : public Analysis
{
public:
	Ancova(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

#endif // ANCOVA_H
