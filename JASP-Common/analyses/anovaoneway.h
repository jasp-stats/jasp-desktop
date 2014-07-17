#ifndef ANOVAONEWAY_H
#define ANOVAONEWAY_H

#include "analysis.h"

class AnovaOneWay : public Analysis
{
public:
	AnovaOneWay(int id);

protected:

	Options *createOptions() const;

};

#endif // ANOVAONEWAY_H
