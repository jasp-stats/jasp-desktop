#ifndef ANOVAREPEATEDMEASURESSHORT_H
#define ANOVAREPEATEDMEASURESSHORT_H

#include "analysis.h"

class AnovaRepeatedMeasuresShort : public Analysis
{
public:
	AnovaRepeatedMeasuresShort(int id);

protected:
	Options* createOptions() const;
};

#endif // ANOVAREPEATEDMEASURESSHORT_H
