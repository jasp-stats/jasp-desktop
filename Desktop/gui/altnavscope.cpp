#include "altnavscope.h"
#include "altnavpostfixassignmentstrategy.h"
#include "altnavroot.h"

#include <QQmlProperty>
#include <QMetaObject>

#include "log.h"
static int z = 0;

ALTNavScope::ALTNavScope(QObject* _attachee)
	: QObject{nullptr}
{

	postfixBroker = ALTNavPostfixAssignmentStrategy::createStrategy(currentStrategy);
	if(_attachee)
	{
		Log::log() << "!!Created " << z++ << std::endl;
		connect(_attachee, &QObject::destroyed, this, [&]() {setParent(nullptr); deleteLater();});
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
	ALTNavRegistry::getInstance()->removeScope(attachee);
	delete postfixBroker;
}

void ALTNavScope::registerWithParent()
{
	if (root)
		return;

	ALTNavRegistry* reg = ALTNavRegistry::getInstance();
	QObject* parentScope = nullptr;

	if(parentOverride)
		parentScope = reg->getAttachedScope(parentScopeAttachee);

	if(attachee && !parentScope)
	{
		QQuickItem* curr = attachee->parentItem();
		while (curr)
		{
			ALTNavScope* scope = reg->getAttachedScope(curr);
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
		parentScope = reg->getDefaultRoot();
		Log::log() << "!!not found root set for " << requestedPostfix.toStdString() << std::endl;
	}

	if(parentScope != parent())
		setParent(parentScope);
}


void ALTNavScope::traverse(QString input)
{
	ALTNavRegistry* reg = ALTNavRegistry::getInstance();

	bool matchPossible = false;
	Log::log() << "!!traverse children " << requestedPostfix.toStdString() << " " << children().length() << std::endl;
	for(QObject* child : children())
	{
		ALTNavScope* scope = qobject_cast<ALTNavScope*>(child);
		//total match, progress in tree
		if(scope->prefix == input)
		{
			scope->match();
			reg->setCurrentNode(scope);
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
		reg->setCurrentNode(reg->getCurrentRoot());
		reg->setAltNavEnabled(false);
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
	setChildrenPrefix();
}

void ALTNavScope::setChildrenPrefix()
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

void ALTNavScope::setForeground(bool onForeground)
{
	if(!root || onForeground == foreground)
		return;

	ALTNavRegistry* reg = ALTNavRegistry::getInstance();
	foreground = onForeground;
	if (onForeground) // set ourselfs as the currentRoot
	{
		reg->setCurrentRoot(this);
		reg->setAltNavInput(prefix);
	}
	else if (ALTNavRegistry::getInstance()->getCurrentRoot() == this) //we are the current root but we lost foreground so unset ourselfs
	{
		reg->setCurrentRoot(reg->getDefaultRoot());
		reg->resetAltNavInput();
	}
	emit foregroundChanged();
}

void ALTNavScope::setRoot(bool value)
{
	root = value;
	if (root)
	{
		Log::log() << "!! root prefix: " << prefix.toStdString() << std::endl;
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

void ALTNavScope::setScopeOnly(bool value)
{
	scopeOnly = value;
	propagateActivity = true;
	emit scopeOnlyChanged();
}


void ALTNavScope::childEvent(QChildEvent* event)
{
	QObject::childEvent(event);

	ALTNavRegistry* reg = ALTNavRegistry::getInstance();
	if(!reg->dynamicTreeUpdate())
		return;

	if(event->added() || event->removed())
	{
		reg->getCurrentNode()->setChildrenActive(reg->AltNavEnabled());
		setChildrenPrefix();
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
