#ifndef CHANGEJS_H
#define CHANGEJS_H

#include "changebase.h"

namespace Modules
{

class ChangeJS : public ChangeBase
{
	Q_OBJECT
	Q_PROPERTY(QString	name		READ name		WRITE setName		NOTIFY nameChanged)
	Q_PROPERTY(QJSValue jsFunction	READ jsFunction	WRITE setJsFunction	NOTIFY jsFunctionChanged)
public:
	ChangeJS();

	void applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const override;

	QString		toString()		const override	{ return _toString() + " running javascript function for option '" + name() + "', function: '" + jsFunction().toString() + "'"; };
	QString		name()			const			{ return _name;		}
	QJSValue	jsFunction()	const			{ return _jsFunction;	}

public slots:
	void setName(			QString		name		);
	void setJsFunction(		QJSValue	jsFunction	);

signals:
	void nameChanged(		QString		name);
	void jsFunctionChanged();

private:
	QString		_name;
	QJSValue	_jsFunction;
};

}

#endif // CHANGEJS_H
