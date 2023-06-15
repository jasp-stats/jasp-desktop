#include "altnavcontrol.h"


ALTNavControl* ALTNavControl::_instance = nullptr;

ALTNavControl* ALTNavControl::ctrl()
{
	if(!_instance)
		_instance = new ALTNavControl();
	return _instance;
}

ALTNavControl::ALTNavControl(QObject *parent) : QObject{parent}
{
	qApp->installEventFilter(this);
	_currentRoot =_currentNode = _defaultRoot = new ALTNavScope(this);
	_defaultRoot->setScopeOnly(true);
	_defaultRoot->setRoot(true);
	_attachedScopeMap.insert(nullptr, _defaultRoot);
}

void ALTNavControl::enableAlTNavigation(bool state)
{
	if(!state)
		setAltNavActive(false);
	_altNavEnabled = state;
}

ALTNavControl::~ALTNavControl()
{

}

ALTNavScope* ALTNavControl::getAttachedScope(QObject *obj)
{
	auto it = _attachedScopeMap.find(obj);
	if (it != _attachedScopeMap.end())
		return it.value();
	return nullptr;
}

void ALTNavControl::registrate(ALTNavScope* scope, QObject *obj)
{
	_attachedScopeMap.insert(obj, scope);
}

void ALTNavControl::unregister(QObject *obj)
{
	auto it = _attachedScopeMap.find(obj);
	if (it != _attachedScopeMap.end())
	{
		ALTNavScope* scope = it.value();
		if (scope == _currentRoot)
		{
			setCurrentRoot(_defaultRoot);
			resetAltNavInput();
		}
		if (scope == _currentNode)
		{
			setCurrentNode(_currentRoot);
			setAltNavInput(_currentRoot->prefix());
		}
	}
	_attachedScopeMap.remove(obj);
}

bool ALTNavControl::eventFilter(QObject *object, QEvent *event)
{
	static bool specialCharInput = false;

	if (event->type() == QEvent::KeyRelease)
	{
		QKeyEvent* keyEvent = static_cast<QKeyEvent *>(event);
		int key = keyEvent->key();
		if (key == Qt::Key_Alt)
		{
			if(!specialCharInput && keyEvent->modifiers() == Qt::NoModifier)
			{
				resetAltNavInput();
				setAltNavActive(!_altNavActive);
			}
			else
				specialCharInput = false;
			return true;

		}
	}
	else if (event->type() == QEvent::KeyPress)
	{
		QKeyEvent* keyEvent = static_cast<QKeyEvent *>(event);
		int key = keyEvent->key();
		if(_altNavActive)
		{
			if ((key >= Qt::Key_A && key <= Qt::Key_Z) || (key >= Qt::Key_0 && key <= Qt::Key_9))
				updateAltNavInput(keyEvent->text().toUpper());
			else if (key != Qt::Key_Alt)
			{
				resetAltNavInput();
				setAltNavActive(false);
			}
			return true;
		}
		else if ((key >= Qt::Key_A && key <= Qt::Key_Z) || (key >= Qt::Key_0 && key <= Qt::Key_9) & keyEvent->modifiers() == Qt::AltModifier)
			specialCharInput = true;
	}
	return false;
}

void ALTNavControl::resetAltNavInput()
{
	_currenAltNavInput = "";
	emit altNavInputChanged();
}

void ALTNavControl::setAltNavInput(QString input)
{
	_currenAltNavInput = input;
	emit altNavInputChanged();
}


void ALTNavControl::updateAltNavInput(QString entry)
{
	_currenAltNavInput += entry;
	_currentNode->traverse(_currenAltNavInput);
	emit altNavInputChanged();
}

void ALTNavControl::setAltNavActive(bool value)
{
	if(!_altNavEnabled || value == _altNavActive) return;

	_altNavActive = value;
	emit altNavActiveChanged();

	if(_altNavActive)
	{
		_dynamicTreeUpdate = true;
		_currentRoot->setChildrenPrefix();
		for(ALTNavScope* node : qAsConst(_attachedScopeMap))
		{
			if (node->foreground())
			{
				setAltNavInput(node->prefix());
				setCurrentNode(node);
			}
		}
		_currentNode->setChildrenActive(true);
	}
	else
	{
		_dynamicTreeUpdate = false;
		_currentNode->setChildrenActive(false);
		setCurrentNode(_currentRoot);
	}
}

bool ALTNavControl::AltNavActive()
{
	return _altNavActive;
}

void ALTNavControl::setCurrentNode(ALTNavScope *scope)
{
	_currentNode->setChildrenActive(false);
	scope->setChildrenActive(_altNavActive);
	scope->setChildrenPrefix();
	_currentNode = scope;
}

void ALTNavControl::setCurrentRoot(ALTNavScope *root)
{
	_currentRoot = root;
}

ALTNavScope *ALTNavControl::getCurrentNode()
{
	return _currentNode;
}

ALTNavScope *ALTNavControl::getCurrentRoot()
{
	return _currentRoot;
}

ALTNavScope *ALTNavControl::getDefaultRoot()
{
	return _defaultRoot;
}

QString ALTNavControl::getCurrentALTNavInput()
{
	return _currenAltNavInput;
}

bool ALTNavControl::dynamicTreeUpdate()
{
	return _dynamicTreeUpdate;
}
