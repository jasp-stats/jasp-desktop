#ifndef CHANGEREMOVE_H
#define CHANGEREMOVE_H

#include "changebase.h"

namespace Modules
{


class ChangeRemove : public ChangeBase
{
	Q_OBJECT
	Q_PROPERTY(QString	name		READ name		WRITE setName	NOTIFY nameChanged)

public:
	ChangeRemove();

	void applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const override;

	QString	name()		const			{ return _name; }
	QString	toString()	const override	{ return _toString() + " removing option '" + name() + "'"; };

public slots:
	void setName(QString name);

signals:
	void nameChanged(QString name);

private:
	QString _name;
};

}

#endif // CHANGEREMOVE_H
