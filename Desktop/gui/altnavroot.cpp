#include "altnavroot.h"
#include "log.h"

ALTNavRegistry* ALTNavRegistry::instance = nullptr;

ALTNavRegistry* ALTNavRegistry::getInstance()
{
	if(!instance)
		instance = new ALTNavRegistry();
	return instance;
}

ALTNavRegistry::ALTNavRegistry(QObject *parent) : QObject{parent}
{
	qApp->installEventFilter(this);
	currentRoot = currentNode = defaultRoot = new ALTNavScope(this);
	defaultRoot->setScopeOnly(true);
	defaultRoot->setRoot(true);
}

ALTNavScope *ALTNavRegistry::getAttachedScope(QObject *obj)
{
	auto it = attachedScopeMap.find(obj);
	if (it != attachedScopeMap.end())
		return it.value();
	return nullptr;
}

void ALTNavRegistry::registerScope(ALTNavScope* scope, QObject *obj)
{
	attachedScopeMap.insert(obj, scope);
}

void ALTNavRegistry::removeScope(QObject *obj)
{
	attachedScopeMap.remove(obj);
}

bool ALTNavRegistry::eventFilter(QObject *object, QEvent *event)
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

void ALTNavRegistry::resetAltNavInput()
{
	currenAltNavInput = "";
	emit altNavInputChanged();
}

void ALTNavRegistry::setAltNavInput(QString input)
{
	currenAltNavInput = input;
	emit altNavInputChanged();
}


void ALTNavRegistry::updateAltNavInput(QString entry)
{
	currenAltNavInput += entry;
	currentNode->traverse(currenAltNavInput);
	emit altNavInputChanged();
}

void ALTNavRegistry::setAltNavEnabled(bool value)
{
	Log::log() << "!!ALTNavEnabled: " << value << std::endl;
	if(value == altNavEnabled) return;

	altNavEnabled = value;
	emit altNavEnabledChanged();

	if(altNavEnabled)
	{
		_dynamicTreeUpdate = true;
		currentNode->setChildrenActive(true);
		currentNode->setChildrenPrefix();
		currentNode->traverse(currenAltNavInput);
	}
	else
	{
		_dynamicTreeUpdate = false;
		currentNode->setChildrenActive(false);
		setCurrentNode(currentRoot);
	}
}

void ALTNavRegistry::setCurrentNode(ALTNavScope *scope)
{
	currentNode->setChildrenActive(false);
	scope->setChildrenActive(altNavEnabled);
	currentNode = scope;
}

void ALTNavRegistry::setCurrentRoot(ALTNavScope *root)
{
	currentRoot = root;
	setCurrentNode(root);
}

ALTNavScope *ALTNavRegistry::getCurrentNode()
{
	return currentNode;
}

ALTNavScope *ALTNavRegistry::getCurrentRoot()
{
	return currentRoot;
}

ALTNavScope *ALTNavRegistry::getDefaultRoot()
{
	return defaultRoot;
}

QString ALTNavRegistry::getCurrentALTNavInput()
{
	return currenAltNavInput;
}

bool ALTNavRegistry::dynamicTreeUpdate()
{
	return _dynamicTreeUpdate;
}
