#ifndef BOUNDMODEL_H
#define BOUNDMODEL_H

#include "availablefields.h"

class BoundModel
{
public:
	BoundModel();

	void setAvailableFields(AvailableFields *available);

	//virtual std::vector<std::string> &exclusiveAssignments();
	virtual void bindTo(Option *) { }
	virtual void bindTo(Option *, int) { }

protected:
	AvailableFields *_availableFields;

};

#endif // BOUNDMODEL_H
