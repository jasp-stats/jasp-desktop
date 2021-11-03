#ifndef CHANGERENAME_H
#define CHANGERENAME_H

#include "changebase.h"

namespace Modules
{

///
/// This change allows one to rename an option.
class ChangeRename : public ChangeBase
{
	Q_OBJECT
	Q_PROPERTY(QString from READ from	WRITE setFrom	NOTIFY fromChanged	)
	Q_PROPERTY(QString to	READ to		WRITE setTo		NOTIFY toChanged	)

public:
	ChangeRename();

	void applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const override;

	QString from()	const { return _from;	}
	QString to()	const { return _to;		}
	
	QString	toString()	const override	{ return _toString() + " renaming option '" + from() + "' to '" + to() + "'"; };

public slots:
	void setFrom(	QString from);
	void setTo(		QString to);

signals:
	void fromChanged(	QString from);
	void toChanged(		QString to);

private:
	QString _from,
			_to;
};

}

#endif // CHANGERENAME_H
