#ifndef CROSSTABS_H
#define CROSSTABS_H

#include "analysis.h"
#include "common.h"

class Crosstabs : public Analysis
{
public:
	Crosstabs(int id);

protected:
	Options *createOptions() const;

};

#endif // CROSSTABS_H
