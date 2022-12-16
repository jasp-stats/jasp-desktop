#include "altnavscope.h"
#include "altnavpostfixassignmentstrategy.h"
#include "altnavcontrol.h"

#include <QQmlProperty>
#include <QMetaObject>

ALTNavScope::ALTNavScope(QObject* attachee)
	: QObject{attachee}
{
	_postfixBroker = ALTNavPostfixAssignmentStrategy::createStrategy(_currentStrategy);
	if(attachee)
	{
		_attachee = qobject_cast<QQuickItem*>(attachee);
		if(_attachee) //is a visual item
		{
			//create a visual tag
			QQmlComponent component(qmlEngine(_attachee), QUrl("qrc:///components/JASP/Widgets/ALTNavTag.qml"), _attachee);
			_attachedTag = qobject_cast<ALTNavTagBase*>(component.create());
			_attachedTag->setParentItem(_attachee);
			_attachedTag->setParent(_attachee);

			//Find parent when attachee parent changes or component is completed (this is when registration of any parent is guaranteed)
			QObject* attached_component = qmlAttachedPropertiesObject<QQmlComponent>(_attachee);
			connect(attached_component, SIGNAL(completed()), this, SLOT(init()));
			connect(_attachee, &QQuickItem::parentChanged, this, &ALTNavScope::registerWithParent);
		}
	}
}

ALTNavScope::~ALTNavScope()
{
	setParentScope(nullptr);
	for (ALTNavScope* child : _childScopes)
		child->setParentScope(nullptr);
	ALTNavControl::getInstance()->unregister(_attachee);
	delete _postfixBroker;
}

void ALTNavScope::registerWithParent()
{
	if (_root)
		return;

	ALTNavControl* ctrl = ALTNavControl::getInstance();
	ALTNavScope* parentScope = nullptr;

	if(_parentOverride)
		parentScope = ctrl->getAttachedScope(_parentScopeAttachee);

	//no parent scope was set explicitly so lets find one
	if(_attachee && !parentScope)
	{
		QQuickItem* curr = _attachee->parentItem();
		while (curr)
		{
			ALTNavScope* scope = ctrl->getAttachedScope(curr);
			if(scope) //found a different valid valid scope
			{
				parentScope = scope;
				break;
			}
			curr = curr->parentItem();
		}
	}

	//no ancestors were registered so set default root
	if(!parentScope)
	{
		parentScope = ctrl->getDefaultRoot();
	}

	if(parentScope != parent())
		setParentScope(parentScope);
}

void ALTNavScope::addChild(ALTNavScope *child)
{
	_childScopes.push_back(child);
	ALTNavControl* ctrl = ALTNavControl::getInstance();
	if(ctrl->dynamicTreeUpdate())
	{
		ctrl->getCurrentNode()->setChildrenActive(ctrl->AltNavEnabled());
		setChildrenPrefix();
	}
}

void ALTNavScope::removeChild(ALTNavScope *child)
{
	_childScopes.removeOne(child);
	ALTNavControl* ctrl = ALTNavControl::getInstance();
	if(ctrl->dynamicTreeUpdate())
	{
		ctrl->getCurrentNode()->setChildrenActive(ctrl->AltNavEnabled());
		setChildrenPrefix();
	}
}

void ALTNavScope::setParentScope(ALTNavScope *parent)
{
	if (_parentScope)
		_parentScope->removeChild(this);
	_parentScope = parent;
	if(_parentScope)
		_parentScope->addChild(this);
	else
	{
		setScopeActive(false);
		setChildrenActive(false);
	}
}

void ALTNavScope::init()
{
	registerWithParent();
	_initialized = true;
}


void ALTNavScope::traverse(QString input)
{
	ALTNavControl* ctrl = ALTNavControl::getInstance();

	bool matchPossible = false;
	for(ALTNavScope* scope : qAsConst(_childScopes))
	{
		//total match, progress in tree
		if(scope->_prefix == input)
		{
			ctrl->setCurrentNode(scope);
			scope->match();
			scope->traverse(input);
			return;
		}
		//partial match
		bool partialMatch = input.length() < scope->_prefix.length() && scope->_prefix.first(input.length()) == input;
		if (!partialMatch)
			scope->setScopeActive(false);
		matchPossible |= partialMatch;
	}

	//end of the road so we disable the altnavigation mode and return to root
	if(!matchPossible)
	{
		ctrl->setCurrentNode(ctrl->getCurrentRoot());
		ctrl->setAltNavEnabled(false);
	}

}

void ALTNavScope::match()
{
	emit tagMatch();
}


void ALTNavScope::setPrefix(QString prefix)
{
	_prefix = prefix;
	if (_attachedTag)
		_attachedTag->setFullTag(_prefix);
	setChildrenPrefix();
}

void ALTNavScope::setChildrenPrefix()
{
	if(_postfixBroker)
		_postfixBroker->assignPostfixes(_childScopes, _prefix);
}

void ALTNavScope::setScopeActive(bool value)
{
	_scopeActive = value;
	if (!_scopeOnly)
		_attachedTag->setActive(value);
	if (_propagateActivity)
		setChildrenActive(value);
}

void ALTNavScope::setChildrenActive(bool value)
{
	for(ALTNavScope* child : _childScopes)
	{
		child->setScopeActive(value);
	}
}

void ALTNavScope::setForeground(bool onForeground)
{
	if(onForeground == _foreground)
		return;

	ALTNavControl* ctrl = ALTNavControl::getInstance();
	_foreground = onForeground;
	if (onForeground) // set ourselfs as the currentNode
	{
		if (_root)
			ctrl->setCurrentRoot(this);
		ctrl->setCurrentNode(this);
		ctrl->setAltNavInput(_prefix);
	}
	else
	{
		if (_root && ctrl->getCurrentRoot() == this) //we are the current root but we lost foreground so unset ourselfs
		{
			ctrl->setCurrentRoot(ctrl->getDefaultRoot());
			ctrl->resetAltNavInput();
		}
		if (ctrl->getCurrentNode() == this) //we are the currentNode but lost foreground reset to root
		{
			ctrl->setCurrentNode(ctrl->getCurrentRoot());
			ctrl->setAltNavInput(ctrl->getCurrentRoot()->prefix());
		}
	}
	emit foregroundChanged();
}

void ALTNavScope::setRoot(bool value)
{
	_root = value;
	if (_root)
	{
		setParentScope(nullptr);
		setPrefix("");
	}

	emit rootChanged();
}

void ALTNavScope::setX(qreal x)
{
	_x = x;
	_attachedTag->setX(_x);
}

void ALTNavScope::setY(qreal y)
{
	_y = y;
	_attachedTag->setY(_y);
}

QString ALTNavScope::getRequestedPostfix()
{
	return _requestedPostfix;
}

int ALTNavScope::getScopePriority()
{
	return _scopePriority;
}

int ALTNavScope::getIndex()
{
	return _index;
}

bool ALTNavScope::foreground()
{
	return _foreground;
}

QString ALTNavScope::prefix()
{
	return _prefix;
}

void ALTNavScope::setScopeOnly(bool value)
{
	_scopeOnly = value;
	_propagateActivity = true;
	emit scopeOnlyChanged();
}

void ALTNavScope::setEnabled(bool value)
{
	if(value == _enabled)
		return;

	_enabled = value;
	if(!_enabled)
	{
		setParentScope(nullptr);
	}
	else if(_initialized) //back in business
	{
		registerWithParent();
	}

	emit enabledChanged();
}

void ALTNavScope::setParentAttachee(QObject* parent)
{
	_parentScopeAttachee = qobject_cast<QQuickItem*>(parent);
	_parentOverride = true;
}

void ALTNavScope::setRequestedPostfix(QString postfix)
{
	_requestedPostfix = postfix;
}

void ALTNavScope::setScopePriority(int priority)
{
	_scopePriority = priority;
}

void ALTNavScope::setIndex(int index)
{
	_index = index;
	//indices changed recalc prefixes
	ALTNavScope* parentScope = qobject_cast<ALTNavScope*>(parent());
	if (parentScope)
		parentScope->setChildrenPrefix();
}

void ALTNavScope::setStrategy(AssignmentStrategy strategy)
{
	setStrategy(ALTNavPostfixAssignmentStrategy::createStrategy(strategy));
	emit postfixAssignmentStrategyChanged();
}


void ALTNavScope::setStrategy(ALTNavPostfixAssignmentStrategy* strategy)
{
	delete _postfixBroker;
	_postfixBroker = strategy;
}
