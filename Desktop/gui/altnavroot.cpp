#include "altnavroot.h"

ALTNavRoot* ALTNavRoot::instance = nullptr;

ALTNavRoot::ALTNavRoot(QObject *parent)
	: ALTNavScope{parent}
{
	qApp->installEventFilter(this);
}

void ALTNavRoot::registerTag(ALTNavTag *tagObject)
{
	tags.insert(tagObject);
}

void ALTNavRoot::removeTag(ALTNavTag *tagObject)
{
	tags.remove(tagObject);
}

void ALTNavRoot::updateTag(ALTNavTag *tagObject)
{
	tagObject->setTagText(tagObject->fullTag);
}

void ALTNavRoot::registerScope(ALTNavScope* scope, QObject *obj)
{
	attachedScopeMap.insert(obj, scope);
}

void ALTNavRoot::removeScope(QObject *obj)
{
	attachedScopeMap.remove(obj);
}

bool ALTNavRoot::eventFilter(QObject *object, QEvent *event)
{
	if (event->type() == QEvent::KeyPress)
	{
		QKeyEvent* keyEvent = static_cast<QKeyEvent *>(event);
		int key = keyEvent->key();
		if (!altNavEnabled && key == Qt::Key_Alt)
		{
			setAltNavEnabled(!altNavEnabled);
			resetAltNavInput();
			return true;

		}
		else if(altNavEnabled)
		{
			if (key == Qt::Key_Escape || key == Qt::Key_Alt)
			{
				setAltNavEnabled(false);
				resetAltNavInput();
				return true;
			}
			else if ((key >= Qt::Key_A && key <= Qt::Key_Z) || (key >= Qt::Key_0 && key <= Qt::Key_9))
			{
				updateAltNavInput(keyEvent->text().toUpper());
				return true;
			}
		}
	}
	return false;
}

void ALTNavRoot::resetAltNavInput()
{
	currenAltNavInput = "";
	activeScope = this;
}

void ALTNavRoot::updateAltNavInput(QString entry)
{
	currenAltNavInput += entry;
	activeScope->traverse(currenAltNavInput);
}

void ALTNavRoot::setAltNavEnabled(bool value)
{
	altNavEnabled = value;
	if(altNavEnabled)
	{
		activeScope->setChildrenActive(true);
		activeScope->setChildrenPrefix(prefix);
	}
}

ALTNavRoot* ALTNavRoot::getInstance()
{
	if(!instance)
		instance = new ALTNavRoot;
	return instance;
}
