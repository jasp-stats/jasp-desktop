#ifndef BOUND_H
#define BOUND_H

#include "options/option.h"

class Bound
{
public:
	virtual void bindTo(Option *option) = 0;
	virtual void unbind() { }

};

#endif // BOUND_H
