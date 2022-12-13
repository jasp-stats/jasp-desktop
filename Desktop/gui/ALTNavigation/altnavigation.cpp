#include "altnavigation.h"
#include "altnavcontrol.h"

ALTNavScope* ALTNavigation::qmlAttachedProperties(QObject *object)
{
	ALTNavScope* scope = new ALTNavScope(object);
	ALTNavControl::getInstance()->registerScope(scope, object);
	return scope;
}

void ALTNavigation::registerQMLTypes()
{
	qmlRegisterType<ALTNavigation>								("JASP",		1, 0, "ALTNavigation"									);
	qmlRegisterType<ALTNavTagBase>								("JASP",		1, 0, "ALTNavTagBase"									);
	qmlRegisterUncreatableType<PriorityStrategy>				("JASP",		1, 0, "AssignmentStrategy",				"Can't make it"	);
}
