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
		connect(_attachee, &QObject::destroyed, this, [this]() {delete this;});
		ALTNavRoot::getInstance()->registerScope(this, _attachee);
		attachee = qobject_cast<QQuickItem*>(_attachee);
		if(attachee) //is a visual item
		{
			Log::log() << "!!" << QQmlProperty(attachee, "a").read().toString().toStdString() << std::endl;
			attachedTag = new ALTNavTag(attachee);
			QObject* attached_component = qmlAttachedPropertiesObject<QQmlComponent>(attachee);
			connect(attached_component, SIGNAL(completed()), this, SLOT(registerWithParent()));
			connect(attachee, &QQuickItem::parentChanged, this, &ALTNavScope::registerWithParent);
		}
	}
}

ALTNavScope::~ALTNavScope()
{
	ALTNavRoot::getInstance()->removeScope(attachee);
	delete postfixBroker;
	attachedTag->deleteLater();
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
				Log::log() << "!!found " << requestedPostfix.toStdString() << " " << scope->requestedPostfix.toStdString() << std::endl;
				parentScope = scope;
				break;
			}
			curr = curr->parentItem();
		}
	}


	if(!parentScope)
	{
		parentScope = root;
		Log::log() << "!!not found root set for " << requestedPostfix.toStdString() << std::endl;
	}

	if(parentScope != parent())
		setParent(parentScope);
}


void ALTNavScope::traverse(QString input)
{
	ALTNavRoot* root = ALTNavRoot::getInstance();

	bool matchPossible = false;
	Log::log() << "!!traverse children " << requestedPostfix.toStdString() << " " << children().length() << std::endl;
	for(QObject* child : children())
	{
		ALTNavScope* scope = qobject_cast<ALTNavScope*>(child);
		//total match, progress in tree
		if(scope->prefix == input)
		{
			scope->match();
			root->setActiveNode(scope, true);
			scope->traverse(input);
			return;
		}
		//partial match
		bool partialMatch = input.length() < scope->prefix.length() && scope->prefix.first(input.length()) == input;
		if (!partialMatch)
			scope->setScopeActive(false);
		matchPossible |= partialMatch;
	}

	//end of the road so we disable the mode and return to root
	if(!matchPossible)
	{
		root->setActiveNode(root, true);
		root->setAltNavEnabled(false);
	}

}

void ALTNavScope::match()
{
	Log::log() << "match: " << prefix.toStdString() << std::endl;
	setScopeActive(false);
	emit tagMatch();
}


void ALTNavScope::setPrefix(QString _prefix)
{
	Log::log() << "!!setprefix " << requestedPostfix.toStdString() << " " << prefix.toStdString() << std::endl;
	prefix = _prefix;
	if (attachedTag)
		attachedTag->setFullTag(prefix);
	setChildrenPrefix(prefix);
}

void ALTNavScope::setChildrenPrefix(QString prefix)
{
	if(postfixBroker)
		postfixBroker->assignPostfixes(children(), prefix);
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

void ALTNavScope::setScopeOnly(bool value)
{
	scopeOnly = value;
	propagateActivity = true;
	emit scopeOnlyChanged();
}


void ALTNavScope::childEvent(QChildEvent* event)
{
	QObject::childEvent(event);

	if(!ALTNavRoot::getInstance()->dynamicTreeUpdate())
		return;

	if(event->added())
	{
		qobject_cast<ALTNavScope*>(event->child())->setScopeActive(scopeActive);
		setChildrenPrefix(prefix);
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
	//indices changed recalc prefixes
	ALTNavScope* parentScope = qobject_cast<ALTNavScope*>(parent());
	if	(parentScope)
		parentScope->setChildrenPrefix(parentScope->prefix);
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
