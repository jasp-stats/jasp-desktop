#ifndef BOUND_H
#define BOUND_H

#include <QString>

#include "option.h"
#include "dataset.h"

class Bound
{

public:

	virtual void bindTo(Option *option) = 0;
	virtual void setDataSet(DataSet *) { }

protected:

};

#endif // BOUND_H
