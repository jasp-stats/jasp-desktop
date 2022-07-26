#ifndef CHANGEBASE_H
#define CHANGEBASE_H

#include <QQuickItem>
#include <json/json.h>
#include "upgradeDefinitions.h"
#include <QJSValue>

namespace Modules
{

class Upgrade;

///
/// Base class for all possible changes upgrades can have
/// manages being registered in Upgrade and can contain condition for when to apply it.
/// See the subclasses for actual functional changes, where applyUpgrade has been implemented
class ChangeBase : public QQuickItem
{
	Q_OBJECT
	
	Q_PROPERTY(QJSValue condition	READ condition	WRITE setCondition	NOTIFY conditionChanged	)
	Q_PROPERTY(QString	msg			READ msg		WRITE setMsg		NOTIFY msgChanged		)
	
public:
	ChangeBase();
	~ChangeBase();

	virtual QString			toString() const = 0;
	virtual void			 applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const = 0;
			bool			conditionSatisfied(const Json::Value & options) const;
	
			QJSValue		condition() const	{ return _condition;	}
			QString			module()	const;
			QString			_toString()	const;
			const QString &	msg() const;

			void setMsg(const QString &newMsg);

public slots:
	void setCondition(QJSValue condition);
	
signals:
	void conditionChanged(QJSValue condition);
	
	void msgChanged();

private slots:
	void registerChange(		QQuickItem * parent);

private:
	Upgrade		*	_upgrade	= nullptr;
	QJSValue		_condition	= QJSValue::SpecialValue::NullValue; //If the condition isnt set (or set to Null) it will be interpreted as "True"
	QString			_msg;
};

}

#endif // CHANGEBASE_H
