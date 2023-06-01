#include "altnavigation.h"
#include "altnavcontrol.h"

ALTNavScope* ALTNavigation::qmlAttachedProperties(QObject *object)
{
	ALTNavScope* scope = new ALTNavScope(object);
	ALTNavControl::getInstance()->registrate(scope, object);
	return scope;
}

//Could parameterize module
void ALTNavigation::registerQMLTypes(QString uri)
{
	std::string u = uri.toStdString();
	qmlRegisterType<ALTNavigation>									(u.c_str(),		1, 0, "ALTNavigation"									);
	qmlRegisterType<ALTNavTagBase>									(u.c_str(),		1, 0, "ALTNavTagBase"									);
	qmlRegisterUncreatableType<ALTNavPostfixAssignmentStrategy>		(u.c_str(),		1, 0, "AssignmentStrategy",				"Can't make it"	);
}
