#ifndef ANOVAREPEATEDMEASURES_H
#define ANOVAREPEATEDMEASURES_H

#include "analysis.h"

class AnovaRepeatedMeasures : public Analysis
{
public:
	AnovaRepeatedMeasures(int id);

protected:
	Options* createOptions() const;
};

#endif // ANOVAREPEATEDMEASURES_H
