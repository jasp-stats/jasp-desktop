#ifndef CROSSTABSBAYESIAN_H
#define CROSSTABSBAYESIAN_H

#include "analysis.h"
#include "common.h"

class CrosstabsBayesian : public Analysis
{
public:
	CrosstabsBayesian(int id);

protected:
	Options *createOptions() const;

};

#endif // CROSSTABSBAYESIAN_H
