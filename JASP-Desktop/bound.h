#ifndef BOUND_H
#define BOUND_H

#include "options/option.h"

class Bound
{
public:
	virtual void bindTo(Option *option) = 0;

};

#endif // BOUND_H
