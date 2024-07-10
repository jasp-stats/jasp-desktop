#ifndef CHANGEJS_H
#define CHANGEJS_H

#include "changebase.h"

namespace Modules
{

///
/// This change is one of the more error-prone ones and enables the use of running custom code
/// It will create a new entry under `name` and then run `jsFunction` with the entire list of `options` as they exist at that time in the update process
class ChangeJS : public ChangeBase
{
	Q_OBJECT
	Q_PROPERTY(QString	name			READ name			WRITE setName			NOTIFY nameChanged)
	Q_PROPERTY(QJSValue jsFunction		READ jsFunction		WRITE setJsFunction		NOTIFY jsFunctionChanged)
	Q_PROPERTY(QJSValue jsonFunction	READ jsFunction		WRITE setJsFunction		NOTIFY jsFunctionChanged) //lets make things robust
	Q_PROPERTY(bool		isNewOption		READ isNewOption	WRITE setIsNewOption	NOTIFY isNewOptionChanged)

public:
	ChangeJS();

	void applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const override;

	QString		toString()		const override	{ return _toString() + " running javascript function for option '" + name() + "', function: '" + jsFunction().toString() + "'"; };
	QString		name()			const			{ return _name;		}
	QJSValue	jsFunction()	const			{ return _jsFunction;	}
	bool		isNewOption()	const			{ return _isNewOption;	}

public slots:
	void setName(			QString		name		);
	void setJsFunction(		QJSValue	jsFunction	);
	void setIsNewOption(	bool		isNewOption	);

signals:
	void nameChanged(		QString		name);
	void jsFunctionChanged();
	void isNewOptionChanged();

private:
	QString		_name;
	QJSValue	_jsFunction;
	bool		_isNewOption = false;
};

}

#endif // CHANGEJS_H
