#include "changeremove.h"
#include "qutils.h"

namespace Modules
{

ChangeRemove::ChangeRemove()
{

}

void ChangeRemove::applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const
{
	const std::string name = fq(_name);

	if(!options.isMember(name))
		throw upgradeError("Could not erase option '" + name + "' because options does not contain it.");

	options.removeMember(name);
	msgs.erase(name);

	msgs[logId].push_back(prefixLog + "Removed option '" + name + "'");
}

void ChangeRemove::setName(QString name)
{
	if (_name == name)
		return;

	_name = name;
	emit nameChanged(_name);
}

}
