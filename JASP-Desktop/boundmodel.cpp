#include "boundmodel.h"

BoundModel::BoundModel()
{
	_availableFields = NULL;
}

void BoundModel::setAvailableFields(AvailableFields *available)
{
	_availableFields = available;
}
