#include "altnavtag.h"
#include "altnavroot.h"

#include <QQmlEngine>
#include <QVariantMap>
#include <QQuickWindow>

#include "log.h"

ALTNavTag::ALTNavTag(QQuickItem *_attachee) : QObject{_attachee}
{
	//create a visual object for which we are the interface
	QQmlComponent component(qmlEngine(_attachee), QUrl("qrc:///components/JASP/Widgets/ALTNavTag.qml"), this);
	QObject* obj = component.createWithInitialProperties(QVariantMap{{"tagData", QVariant::fromValue<ALTNavTag*>(this)}});
	tagItem = qobject_cast<QQuickItem*>(obj);
	attachee = _attachee;
	tagItem->setParentItem(attachee);

	//connect to root to get input updates so we may update tag accordingly
	connect(ALTNavRegistry::getInstance(), &ALTNavRegistry::altNavInputChanged, this, &ALTNavTag::updateTagText);
}

ALTNavTag::~ALTNavTag()
{
	delete tagItem;
}

void ALTNavTag::setFullTag(QString _fullTag)
{
	fullTag = _fullTag;
	updateTagText();
}


void ALTNavTag::updateTagText()
{
	if(active)
	{
		int len = fullTag.length() - ALTNavRegistry::getInstance()->getCurrentALTNavInput().length();
		if (len >= 0)
		{
			tagText = fullTag.last(len);
			emit tagTextChanged();
		}
	}
}

void ALTNavTag::setActive(bool _active)
{
	active = _active;	
	position();
	emit activeChanged();
}

void ALTNavTag::setX(qreal _x)
{
	x = _x;
	if(active)
		position();
}

void ALTNavTag::setY(qreal _y)
{
	y = _y;
	if(active)
		position();
}

void ALTNavTag::position()
{
//	tagItem->setParentItem(attachee->window()->contentItem());
//	QPointF coord = attachee->mapToScene(QPointF(x, y));
	tagItem->setX(x);//coord.x());
	tagItem->setY(y);//coord.y());
}
