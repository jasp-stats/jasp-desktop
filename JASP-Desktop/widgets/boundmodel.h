#ifndef BOUNDMODEL_H
#define BOUNDMODEL_H

#include "options/option.h"

class BoundModel
{
public:
	virtual void bindTo(Option *) { }
	//virtual void bindTo(Option *, int) { }

};

#endif // BOUNDMODEL_H
