#include "altnavigation.h"
#include "altnavcontrol.h"

ALTNavScope* ALTNavigation::qmlAttachedProperties(QObject *object)
{
	ALTNavScope* scope = new ALTNavScope(object);
	ALTNavControl::ctrl()->registrate(scope, object);
	return scope;
}
