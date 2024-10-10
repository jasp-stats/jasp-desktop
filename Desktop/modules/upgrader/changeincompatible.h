#ifndef CHANGEINCOMPATIBLE_H
#define CHANGEINCOMPATIBLE_H

#include "changebase.h"


namespace Modules
{

/// Removes all options and adds a message that from version * onwards it was reset.
class ChangeIncompatible : public ChangeBase
{
	Q_OBJECT
public:
	ChangeIncompatible();
	
	void applyUpgrade(Json::Value & options, UpgradeMsgs & msgs, bool inMeta) const override;
	
	QString	toString()	const override	{ return _toString() + " removing all options, because '" + msg() + "'"; };
};


}

#endif // CHANGEINCOMPATIBLE_H
