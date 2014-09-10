#ifndef ANOVAREPEATEDMEASURESBAYESIAN_H
#define ANOVAREPEATEDMEASURESBAYESIAN_H

#include "analysis.h"

class AnovaRepeatedMeasuresBayesian : public Analysis
{
public:
	AnovaRepeatedMeasuresBayesian(int id);

protected:
	Options* createOptions() const;
};

#endif // ANOVAREPEATEDMEASURESBAYESIAN_H
