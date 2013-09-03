#ifndef BOUNDMULTI_H
#define BOUNDMULTI_H

#include "option.h"
#include "dataset.h"

class BoundMulti
{
public:
	virtual void bindTo(Option *option, int item) = 0;
	virtual void setDataSet(DataSet *) { }
};

#endif // BOUNDMULTI_H
