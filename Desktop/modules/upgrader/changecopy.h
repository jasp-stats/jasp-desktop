#ifndef CHANGECOPY_H
#define CHANGECOPY_H

#include "changebase.h"

namespace Modules
{

class ChangeCopy : public ChangeBase
{
	Q_OBJECT
	Q_PROPERTY(QString from READ from	WRITE setFrom	NOTIFY fromChanged	)
	Q_PROPERTY(QString to	READ to		WRITE setTo		NOTIFY toChanged	)

public:
	ChangeCopy();

	void	applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const override;
	
	QString	toString()	const override	{ return _toString() + " copying option from '" + from() + "' to '" + to() + "'"; };
	QString from()		const			{ return _from;	}
	QString to()		const			{ return _to;		}

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
#endif // CHANGECOPY_H
