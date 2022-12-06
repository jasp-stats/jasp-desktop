#include "altnavigation.h"
#include "altnavroot.h"

ALTNavScope* ALTNavigation::qmlAttachedProperties(QObject *object)
{
	ALTNavScope* scope = new ALTNavScope(object);
	ALTNavRegistry::getInstance()->registerScope(scope, object);
	return scope;
}
