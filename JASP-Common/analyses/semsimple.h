#ifndef SEMSIMPLE_H
#define SEMSIMPLE_H

#include "analysis.h"

class SEMSimple : public Analysis
{
public:
	SEMSimple(int id);

protected:
	virtual Options *createDefaultOptions() OVERRIDE;

};

#endif // SEMSIMPLE_H
