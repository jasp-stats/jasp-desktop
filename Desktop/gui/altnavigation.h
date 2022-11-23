#ifndef ALTNAVIGATION_H
#define ALTNAVIGATION_H

#include <QObject>
#include <QQmlEngine>

#include "altnavscope.h"

class ALTNavigation : public QObject
{
	Q_OBJECT
	QML_ATTACHED(ALTNavScope)

public:
	static ALTNavScope* qmlAttachedProperties(QObject *object);

};

#endif // ALTNAVIGATION_H
