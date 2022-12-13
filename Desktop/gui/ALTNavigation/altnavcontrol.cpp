#include "altnavcontrol.h"

ALTNavControl* ALTNavControl::instance = nullptr;

ALTNavControl* ALTNavControl::getInstance()
{
	if(!instance)
		instance = new ALTNavControl();
	return instance;
}

ALTNavControl::ALTNavControl(QObject *parent) : QObject{parent}
{
	qApp->installEventFilter(this);
	currentRoot = currentNode = defaultRoot = new ALTNavScope(this);
	defaultRoot->setScopeOnly(true);
	defaultRoot->setRoot(true);
	attachedScopeMap.insert(nullptr, defaultRoot);
}

ALTNavControl::~ALTNavControl()
{

}

ALTNavScope* ALTNavControl::getAttachedScope(QObject *obj)
{
	auto it = attachedScopeMap.find(obj);
	if (it != attachedScopeMap.end())
		return it.value();
	return nullptr;
}

void ALTNavControl::registrate(ALTNavScope* scope, QObject *obj)
{
	attachedScopeMap.insert(obj, scope);
}

void ALTNavControl::unregister(QObject *obj)
{
	attachedScopeMap.remove(obj);
}

bool ALTNavControl::eventFilter(QObject *object, QEvent *event)
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

void ALTNavControl::resetAltNavInput()
{
	currenAltNavInput = "";
	emit altNavInputChanged();
}

void ALTNavControl::setAltNavInput(QString input)
{
	currenAltNavInput = input;
	emit altNavInputChanged();
}


void ALTNavControl::updateAltNavInput(QString entry)
{
	currenAltNavInput += entry;
	currentNode->traverse(currenAltNavInput);
	emit altNavInputChanged();
}

void ALTNavControl::setAltNavEnabled(bool value)
{
	if(value == altNavEnabled) return;

	altNavEnabled = value;
	emit altNavEnabledChanged();

	if(altNavEnabled)
	{
		_dynamicTreeUpdate = true;
		currentRoot->setChildrenPrefix();
		for(ALTNavScope* node : qAsConst(attachedScopeMap))
		{
			if (node->foreground())
			{
				setAltNavInput(node->prefix());
				setCurrentNode(node);
			}
		}
		currentNode->setChildrenActive(true);
	}
	else
	{
		_dynamicTreeUpdate = false;
		currentNode->setChildrenActive(false);
		setCurrentNode(currentRoot);
	}
}

bool ALTNavControl::AltNavEnabled()
{
	return altNavEnabled;
}

void ALTNavControl::setCurrentNode(ALTNavScope *scope)
{
	currentNode->setChildrenActive(false);
	scope->setChildrenActive(altNavEnabled);
	scope->setChildrenPrefix();
	currentNode = scope;
}

void ALTNavControl::setCurrentRoot(ALTNavScope *root)
{
	currentRoot = root;
}

ALTNavScope *ALTNavControl::getCurrentNode()
{
	return currentNode;
}

ALTNavScope *ALTNavControl::getCurrentRoot()
{
	return currentRoot;
}

ALTNavScope *ALTNavControl::getDefaultRoot()
{
	return defaultRoot;
}

QString ALTNavControl::getCurrentALTNavInput()
{
	return currenAltNavInput;
}

bool ALTNavControl::dynamicTreeUpdate()
{
	return _dynamicTreeUpdate;
}
