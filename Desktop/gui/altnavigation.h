#ifndef ALTNAVIGATION_H
#define ALTNAVIGATION_H

#include <QObject>
#include <QQmlEngine>

#include "altnavscope.h"

class ALTNavigation : public QObject
{
	Q_OBJECT
	QML_ATTACHED(ALTNavScope)
	QML_ELEMENT

public:

	static ALTNavScope* qmlAttachedProperties(QObject *object);

signals:

};

#endif // ALTNAVIGATION_H
