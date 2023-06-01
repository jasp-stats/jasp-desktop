#ifndef CHANGEREMOVE_H
#define CHANGEREMOVE_H

#include "changebase.h"

namespace Modules
{

///
/// This changes simply removes an option entirely, not really needed because it would then be ignored anyway.
/// Perhaps if the original option contains faulty information or something this could be useful?
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
