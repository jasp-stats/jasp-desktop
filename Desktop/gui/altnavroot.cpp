#include "altnavroot.h"
#include "log.h"

ALTNavRoot* ALTNavRoot::instance = nullptr;

ALTNavRoot::ALTNavRoot(QObject *parent)
	: ALTNavScope{parent}
{
	qApp->installEventFilter(this);
	setScopeOnly(true);
}

ALTNavScope *ALTNavRoot::getAttachedScope(QObject *obj)
{
	auto it = attachedScopeMap.find(obj);
	if (it != attachedScopeMap.end())
		return it.value();
	return nullptr;
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
			resetAltNavInput();
			setAltNavEnabled(!altNavEnabled);
			return true;

		}
		else if(altNavEnabled)
		{
			if ((key >= Qt::Key_A && key <= Qt::Key_Z) || (key >= Qt::Key_0 && key <= Qt::Key_9))
			{
				updateAltNavInput(keyEvent->text().toUpper());
				return true;
			}
			else
			{
				resetAltNavInput();
				setAltNavEnabled(false);
				return true;
			}
		}
	}
	return false;
}

void ALTNavRoot::resetAltNavInput()
{
	currenAltNavInput = "";
	emit altNavInputChanged();
}

void ALTNavRoot::updateAltNavInput(QString entry)
{
	currenAltNavInput += entry;
	activeNode->traverse(currenAltNavInput);
	emit altNavInputChanged();
}

void ALTNavRoot::setAltNavEnabled(bool value)
{
	Log::log() << "!!ALTNavEnabled: " << value << std::endl;
	altNavEnabled = value;
	if(altNavEnabled)
	{
		_dynamicTreeUpdate = true;
		activeNode->setChildrenActive(true);
		activeNode->setChildrenPrefix(prefix);
		activeNode->traverse(currenAltNavInput);
	}
	else
	{
		_dynamicTreeUpdate = false;
		activeNode->setChildrenActive(false);
		setActiveNode(this);
	}
}

void ALTNavRoot::setActiveNode(ALTNavScope *scope)
{
	activeNode = scope;
}

ALTNavScope *ALTNavRoot::getActiveNode()
{
	return activeNode;
}

QString ALTNavRoot::getCurrentALTNavInput()
{
	return currenAltNavInput;
}

bool ALTNavRoot::dynamicTreeUpdate()
{
	return _dynamicTreeUpdate;
}

ALTNavRoot* ALTNavRoot::getInstance()
{
	if(!instance)
		instance = new ALTNavRoot;
	return instance;
}
