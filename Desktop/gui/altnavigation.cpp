#include "altnavigation.h"

ALTNavScope* ALTNavigation::qmlAttachedProperties(QObject *object)
{

	return new ALTNavScope(object);
}
