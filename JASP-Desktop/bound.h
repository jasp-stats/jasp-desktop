#ifndef BOUND_H
#define BOUND_H

#include <QString>

#include "option.h"

class Bound
{

public:

	virtual void bindTo(Option *option) = 0;

protected:

};

#endif // BOUND_H
