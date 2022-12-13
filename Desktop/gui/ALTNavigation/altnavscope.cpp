#include "altnavscope.h"
#include "altnavpostfixassignmentstrategy.h"
#include "altnavcontrol.h"

#include <QQmlProperty>
#include <QMetaObject>

ALTNavScope::ALTNavScope(QObject* _attachee)
	: QObject{nullptr}
{

	postfixBroker = ALTNavPostfixAssignmentStrategy::createStrategy(currentStrategy);
	if(_attachee)
	{
		connect(_attachee, &QObject::destroyed, this, [&]() {setParent(nullptr); deleteLater();});
		attachee = qobject_cast<QQuickItem*>(_attachee);
		if(attachee) //is a visual item
		{
			//create a visual tag
			QQmlComponent component(qmlEngine(attachee), QUrl("qrc:///components/JASP/Widgets/ALTNavTag.qml"), attachee);
			attachedTag = qobject_cast<ALTNavTagBase*>(component.create());
			attachedTag->setParentItem(attachee);
			attachedTag->setParent(attachee);

			QObject* attached_component = qmlAttachedPropertiesObject<QQmlComponent>(attachee);
			connect(attached_component, SIGNAL(completed()), this, SLOT(init()));
			connect(attachee, &QQuickItem::parentChanged, this, &ALTNavScope::registerWithParent);
		}
	}
}

ALTNavScope::~ALTNavScope()
{
	ALTNavControl::getInstance()->removeScope(attachee);
	delete postfixBroker;
}

void ALTNavScope::registerWithParent()
{
	if (root)
		return;

	ALTNavControl* ctrl = ALTNavControl::getInstance();
	QObject* parentScope = nullptr;

	if(parentOverride)
		parentScope = ctrl->getAttachedScope(parentScopeAttachee);

	if(attachee && !parentScope)
	{
		QQuickItem* curr = attachee->parentItem();
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


	if(!parentScope)
	{
		parentScope = ctrl->getDefaultRoot();
	}

	if(parentScope != parent())
		setParent(parentScope);
}

void ALTNavScope::init()
{
	registerWithParent();
	initialized = true;
}


void ALTNavScope::traverse(QString input)
{
	ALTNavControl* ctrl = ALTNavControl::getInstance();

	bool matchPossible = false;
	for(QObject* child : children())
	{
		ALTNavScope* scope = qobject_cast<ALTNavScope*>(child);
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

	//end of the road so we disable the mode and return to root
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
	if (attachedTag)
		attachedTag->setFullTag(_prefix);
	setChildrenPrefix();
}

void ALTNavScope::setChildrenPrefix()
{
	if(postfixBroker)
		postfixBroker->assignPostfixes(children(), _prefix);
}

void ALTNavScope::setScopeActive(bool value)
{
	scopeActive = value;
	if (!scopeOnly)
		attachedTag->setActive(value);
	if (propagateActivity)
		setChildrenActive(value);
}

void ALTNavScope::setChildrenActive(bool value)
{
	for(auto child : children())
	{
		qobject_cast<ALTNavScope*>(child)->setScopeActive(value);
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
		if (root)
			ctrl->setCurrentRoot(this);
		ctrl->setCurrentNode(this);
		ctrl->setAltNavInput(_prefix);
	}
	else if (ctrl->getCurrentNode() == this) //we are the current root but we lost foreground so unset ourselfs
	{
		ctrl->setCurrentNode(ctrl->getCurrentRoot());
		ctrl->setAltNavInput(ctrl->getCurrentRoot()->prefix());
	}
	else if (root && ctrl->getCurrentRoot() == this)
	{
		ctrl->setCurrentRoot(ctrl->getDefaultRoot());
		ctrl->resetAltNavInput();
	}
	emit foregroundChanged();
}

void ALTNavScope::setRoot(bool value)
{
	root = value;
	if (root)
	{
		setParent(nullptr);
		setPrefix("");
	}

	emit rootChanged();
}

void ALTNavScope::setX(qreal _x)
{
	x = _x;
	attachedTag->setX(x);
}

void ALTNavScope::setY(qreal _y)
{
	y = _y;
	attachedTag->setY(y);
}

QString ALTNavScope::getRequestedPostfix()
{
	return requestedPostfix;
}

int ALTNavScope::getScopePriority()
{
	return scopePriority;
}

int ALTNavScope::getIndex()
{
	return index;
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
	scopeOnly = value;
	propagateActivity = true;
	emit scopeOnlyChanged();
}


void ALTNavScope::childEvent(QChildEvent* event)
{
	QObject::childEvent(event);

	ALTNavControl* ctrl = ALTNavControl::getInstance();
	if(!ctrl->dynamicTreeUpdate())
		return;

	if(event->added() || event->removed())
	{
		ctrl->getCurrentNode()->setChildrenActive(ctrl->AltNavEnabled());
		setChildrenPrefix();
	}
}

void ALTNavScope::setEnabled(bool value)
{
	if(value == enabled)
		return;

	enabled = value;
	if(!enabled)
	{
		setParent(nullptr);
		setScopeActive(false);
	}
	else if(initialized)
	{
		registerWithParent();
	}

	emit enabledChanged();
}

void ALTNavScope::setParentAttachee(QObject* _parent)
{
	parentScopeAttachee = qobject_cast<QQuickItem*>(_parent);
	parentOverride = true;
}

void ALTNavScope::setRequestedPostfix(QString postfix)
{
	requestedPostfix = postfix;
}

void ALTNavScope::setScopePriority(int priority)
{
	scopePriority = priority;
}

void ALTNavScope::setIndex(int _index)
{
	index = _index;
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
	delete postfixBroker;
	postfixBroker = strategy;
}
