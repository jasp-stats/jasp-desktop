#ifndef CHANGESETVALUE_H
#define CHANGESETVALUE_H

#include "changebase.h"
#include "jsonredirect.h"

namespace Modules
{

///
/// This change allows one to set a particular value, which can be any kind of json object.
/// It should of course be exactly that which the QML element expects.
class ChangeSetValue : public ChangeBase
{
	Q_OBJECT
	Q_PROPERTY(QString	name		READ name		WRITE setName		NOTIFY nameChanged)
	Q_PROPERTY(QJSValue jsonValue	READ jsonValue	WRITE setJsonValue	NOTIFY jsonValueChanged) //Aka an object, which isn't converted to JSON by qt/qml but we can do that ourselves
public:
	ChangeSetValue();

	void applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const override;

	QString		toString()	const override;
	QString		name()		const { return _name;		}
	QJSValue	jsonValue()	const;

public slots:
	void setName(		QString name);
	void setJsonValue(	QJSValue jsonValue);

signals:
	void nameChanged(	QString name);
	void jsonValueChanged();

private:
	QString		_name;
	Json::Value	_jsonValue;
};

}

#endif // CHANGESETVALUE_H
