#include "altnavtag.h"
#include "altnavcontrol.h"

#include <QQmlEngine>
#include <QVariantMap>
#include <QQuickWindow>


ALTNavTagBase::ALTNavTagBase(QQuickItem* parent) : QQuickItem{parent}
{
	//connect to control to get input updates so we may update tag accordingly
	connect(ALTNavControl::getInstance(), &ALTNavControl::altNavInputChanged, this, &ALTNavTagBase::updateTagText);
	setActiveFocusOnTab(false);
}

ALTNavTagBase::~ALTNavTagBase()
{

}

void ALTNavTagBase::setFullTag(QString _fullTag)
{
	fullTag = _fullTag;
	updateTagText();
}


void ALTNavTagBase::updateTagText()
{
	int len = fullTag.length() - ALTNavControl::getInstance()->getCurrentALTNavInput().length();
	if (len >= 0)
	{
		tagText = fullTag.last(len);
		emit tagTextChanged();
	}
}

void ALTNavTagBase::setActive(bool _active)
{
	active = _active;	
	emit activeChanged();
}
