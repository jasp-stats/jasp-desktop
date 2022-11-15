#include "altnavtag.h"
#include "altnavroot.h"

#include <QQmlEngine>
#include <QVariantMap>

ALTNavTag::ALTNavTag(QQuickItem *attachee) : QObject{nullptr}
{
	ALTNavRoot::getInstance()->registerTag(this);
	QQmlComponent component(qmlEngine(attachee), QUrl("qrc:///components/JASP/Widgets/ALTNavTag.qml"), this);
	QObject* obj = component.createWithInitialProperties(QVariantMap{{"tagData", QVariant::fromValue<ALTNavTag*>(this)}});
	tagItem = qobject_cast<QQuickItem*>(obj);
	tagItem->setParentItem(attachee);
}

ALTNavTag::~ALTNavTag()
{
	ALTNavRoot::getInstance()->removeTag(this);
}

void ALTNavTag::setFullTag(QString _fullTag)
{
	fullTag = _fullTag;
	ALTNavRoot::getInstance()->updateTag(this);
}


void ALTNavTag::setTagText(QString tag)
{
	tagText = tag;
	emit tagTextChanged();
}

void ALTNavTag::setActive(bool _active)
{
	active = _active;
	emit activeChanged();
}
