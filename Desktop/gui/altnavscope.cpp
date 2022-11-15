#include "altnavscope.h"
#include "altnavpostfixassignmentstrategy.h"
#include "altnavroot.h"

#include <QQmlProperty>

#include "log.h"
static int x = 0;

ALTNavScope::ALTNavScope(QObject* _attachee)
	: QObject{nullptr}
{

	postfixBroker = ALTNavPostfixAssignmentStrategy::createStrategy(currentStrategy);
	if(_attachee)
	{
		index = x++;
		Log::log() << "!!Created " << index << std::endl;
		ALTNavRoot::getInstance()->registerScope(this, _attachee);
		attachee = qobject_cast<QQuickItem*>(_attachee);
		if(attachee) //is a visual item
		{
			Log::log() << "!!" << QQmlProperty(attachee, "a").read().toString().toStdString() << std::endl;
			attachedTag = new ALTNavTag(attachee);
			QObject* attached_component = qmlAttachedPropertiesObject<QQmlComponent>(attachee);
			QObject::connect(attached_component, SIGNAL(completed()), this, SLOT(registerWithParent()));
		}
	}
}

ALTNavScope::~ALTNavScope()
{
	ALTNavRoot::getInstance()->removeScope(attachee);
	delete postfixBroker;
	delete attachedTag;
}

void ALTNavScope::traverse(QString input)
{
	bool matchPossible = false;
	for(QObject* child : children())
	{
		ALTNavScope* scope = qobject_cast<ALTNavScope*>(child);
		if(scope->prefix == input)
		{

		}
		//partial match
		matchPossible |= scope->prefix.length() < input.length() && scope->prefix.first(scope->prefix.length()) == input;
	}

	if(!matchPossible)
		set

}


void ALTNavScope::setPrefix(QString prefix)
{
	prefix = prefix;
	if (attachedTag)
		attachedTag->setFullTag(prefix);
}

void ALTNavScope::setChildrenPrefix(QString tag)
{
	postfixBroker->assignPostfixes(children(), prefix);
}

void ALTNavScope::setChildrenActive(bool value)
{
	for(auto child : children())
	{
		qobject_cast<ALTNavScope*>(child)->setScopeActive(value);
	}
}

void ALTNavScope::setScopeActive(bool value)
{
	scopeActive = value;
	if (attachedTag)
		attachedTag->setActive(value);
}

void ALTNavScope::registerWithParent()
{
	ALTNavRoot* root = ALTNavRoot::getInstance();
	QObject* parentScope = nullptr;

	if(parentOverride)
		parentScope = root->getAttachedScope(parentScopeAttachee);

	if(attachee && !parentScope)
	{
		QQuickItem* curr = attachee->parentItem();
		while (curr)
		{
			ALTNavScope* scope = root->getAttachedScope(curr);
			if(scope) //found a different valid valid scope
			{
				Log::log() << "!!found " << index << " " << scope->index << std::endl;
				parentScope = scope;
				break;
			}
			curr = curr->parentItem();
		}
	}


	if(!parentScope)
	{
		parentScope = root;
		Log::log() << "!!not found root set for " << index << std::endl;
	}

	if(parentScope != parent())
		setParent(parentScope);
}



void ALTNavScope::setScopeOnly(bool value)
{
	//visual tag must be added
	if(!value && scopeOnly)
	{
		QQuickItem* attacheeItem = qobject_cast<QQuickItem*>(attachee);
		if(attacheeItem) //is a visual item
		{
			attachedTag = new ALTNavTag(attacheeItem);
		}
	}

	//visual tag must be deleted
	if(value && !scopeOnly)
	{
		delete attachedTag;
	}

	scopeOnly = value;
	emit scopeOnlyChanged();
}


void ALTNavScope::childEvent(QChildEvent* event)
{
	QObject::childEvent(event);
	if(event->added())
	{
		for(auto child: children())
			qobject_cast<ALTNavScope*>(child)->registerWithParent();
	}
}

void ALTNavScope::setEnabled(bool value)
{
	enabled = value;
	emit enabledChanged();
}

void ALTNavScope::setParentAttachee(QObject* _parent)
{
	parentScopeAttachee = qobject_cast<QQuickItem*>(_parent);
	parentOverride = true;
	//registerWithParent();
}

void ALTNavScope::setRequestedPostfix(QString postfix)
{
	Log::log() << "!!" << postfix.toStdString() << " " << index << std::endl;
	requestedPostfix = postfix;
}

void ALTNavScope::setScopePriority(int priority)
{
	scopePriority = priority;
}

void ALTNavScope::setIndex(int _index)
{
	index = _index;

};

void ALTNavScope::setStrategy(AssignmentStrategy strategy)
{
	setStrategy(ALTNavPostfixAssignmentStrategy::createStrategy(strategy));
	emit postfixAssignmentStrategyChanged();
}


void ALTNavScope::setStrategy(ALTNavPostfixAssignmentStrategy* strategy)
{
	delete postfixBroker;
	postfixBroker =  strategy;
}
