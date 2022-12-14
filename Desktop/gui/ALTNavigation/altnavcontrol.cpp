#include "altnavcontrol.h"

ALTNavControl* ALTNavControl::_instance = nullptr;

ALTNavControl* ALTNavControl::getInstance()
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
	_attachedScopeMap.remove(obj);
}

bool ALTNavControl::eventFilter(QObject *object, QEvent *event)
{
	if (event->type() == QEvent::KeyPress)
	{
		QKeyEvent* keyEvent = static_cast<QKeyEvent *>(event);
		int key = keyEvent->key();
		if (!_altNavEnabled && key == Qt::Key_Alt)
		{
			resetAltNavInput();
			setAltNavEnabled(!_altNavEnabled);
			return true;

		}
		else if(_altNavEnabled)
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

void ALTNavControl::setAltNavEnabled(bool value)
{
	if(value == _altNavEnabled) return;

	_altNavEnabled = value;
	emit altNavEnabledChanged();

	if(_altNavEnabled)
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

bool ALTNavControl::AltNavEnabled()
{
	return _altNavEnabled;
}

void ALTNavControl::setCurrentNode(ALTNavScope *scope)
{
	_currentNode->setChildrenActive(false);
	scope->setChildrenActive(_altNavEnabled);
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
