#include "changeincompatible.h"

namespace Modules
{

ChangeIncompatible::ChangeIncompatible() 
{
	
}

void ChangeIncompatible::applyUpgrade(Json::Value &options, UpgradeMsgs &msgs, bool inMeta) const
{
	options = Json::objectValue;	
	
	if(!inMeta)
		msgs[analysisLog].push_back(prefixLog + tr("Reset analysis because of '%1'").arg(msg().isEmpty() ? "???" : msg()).toStdString());
}

}
