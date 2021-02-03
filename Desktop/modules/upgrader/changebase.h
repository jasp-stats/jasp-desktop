#ifndef CHANGEBASE_H
#define CHANGEBASE_H

#include <QQuickItem>
#include "jsonredirect.h"
#include "upgradeDefinitions.h"
#include <QJSValue>

namespace Modules
{

class Upgrade;

class ChangeBase : public QQuickItem
{
	Q_OBJECT
	
	Q_PROPERTY(QJSValue condition READ condition WRITE setCondition NOTIFY conditionChanged)
	
public:
	ChangeBase();
	~ChangeBase();

	virtual QString		toString() const = 0;
	virtual void		applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const = 0;
			bool		conditionSatisfied(const Json::Value & options) const;
	
			QJSValue	condition() const	{ return _condition;	}
			QString		module()	const;
			QString		_toString()	const;
	
public slots:
	void setCondition(QJSValue condition);
	
signals:
	void conditionChanged(QJSValue condition);
	
private slots:
	void registerChange(		QQuickItem * parent);

private:
	Upgrade		*	_upgrade	= nullptr;
	QJSValue		_condition	= QJSValue::SpecialValue::NullValue; //If the condition isnt set (or set to Null) it will be interpreted as "True"
};

}

#endif // CHANGEBASE_H
