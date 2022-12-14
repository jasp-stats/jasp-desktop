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

void ALTNavTagBase::setFullTag(QString fullTag)
{
	_fullTag = fullTag;
	updateTagText();
}


void ALTNavTagBase::updateTagText()
{
	int len = _fullTag.length() - ALTNavControl::getInstance()->getCurrentALTNavInput().length();
	if (len >= 0)
	{
		_tagText = _fullTag.last(len);
		emit tagTextChanged();
	}
}

void ALTNavTagBase::setActive(bool active)
{
	_active = active;
	emit activeChanged();
}
