#include "altnavtag.h"
#include "altnavroot.h"

#include <QQmlEngine>
#include <QVariantMap>

ALTNavTag::ALTNavTag(QQuickItem *attachee) : QObject{attachee}
{
	//create a visual object for which we are the interface
	QQmlComponent component(qmlEngine(attachee), QUrl("qrc:///components/JASP/Widgets/ALTNavTag.qml"), this);
	QObject* obj = component.createWithInitialProperties(QVariantMap{{"tagData", QVariant::fromValue<ALTNavTag*>(this)}});
	tagItem = qobject_cast<QQuickItem*>(obj);
	tagItem->setParentItem(attachee);

	//connect to root to get input updates so we may update tag accordingly
	connect(ALTNavRoot::getInstance(), &ALTNavRoot::altNavInputChanged, this, &ALTNavTag::updateTagText);
}

ALTNavTag::~ALTNavTag()
{
	tagItem->setParentItem(nullptr);
	tagItem->disconnect();
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
		int len = fullTag.length() - ALTNavRoot::getInstance()->getCurrentALTNavInput().length();
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
	emit activeChanged();
}
