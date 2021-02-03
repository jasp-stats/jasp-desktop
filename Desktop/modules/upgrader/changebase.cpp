#include "utilities/qutils.h"
#include "changebase.h"
#include "upgrade.h"

namespace Modules
{

ChangeBase::ChangeBase()
{
	connect(this, &QQuickItem::parentChanged, this, &ChangeBase::registerChange);
}

void ChangeBase::setCondition(QJSValue condition)
{
	_condition = condition;
	emit conditionChanged(_condition);
}

bool ChangeBase::conditionSatisfied(const Json::Value & options) const
{
	if(_condition.isNull())
		return true;
	
	if(_condition.isCallable())
	{
		QJSValue satisfied = QJSValue(_condition).call({ tqj(options, this) });
		
		if(satisfied.isError())
			throw upgradeError(fq("Javascript condition for change '" + toString() + "' failed with the following type of error: '" + QJSErrorToString(satisfied.errorType()) + "' check the definition."));
		
		if(satisfied.isBool())
			return satisfied.toBool();
	
		throw upgradeError(fq("Javascript condition for change '" + toString() + "' does not return a boolean!."));
	}
	
	if(_condition.isBool()) //Also fine
		return _condition.toBool();
	
	throw upgradeError(fq("Javascript condition for change '" + toString() + "' is not a function, not Null and not a boolean..."));
}

QString ChangeBase::module() const	
{ 
	return _upgrade ? _upgrade->module() : "???"; 
}

QString ChangeBase::_toString() const	
{ 
	return !_upgrade ? "???" : _upgrade->toString();
}

void ChangeBase::registerChange(QQuickItem * parent)
{
	Upgrade * upgrade = dynamic_cast<Upgrade*>(parent);
	
	if(!upgrade && parent)
		throw upgradeLoadError(fq(toString()), "A Change* Item must always be a child of Upgrade");
	
	if(upgrade != _upgrade)
	{
		if(_upgrade)
			_upgrade->removeChange(this);

		_upgrade = upgrade;

		if(_upgrade)
			_upgrade->addChange(this);
	}
}

ChangeBase::~ChangeBase()
{
	if(_upgrade)
		_upgrade->removeChange(this);
}

}
