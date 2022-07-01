#include "changerename.h"
#include "qutils.h"


namespace Modules
{

ChangeRename::ChangeRename()
{

}

void ChangeRename::applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const
{
	const std::string	oldName = fq(_from),
						newName = fq(_to);

	if(options.isMember(newName))
		throw upgradeError("Could not rename option '" + oldName + "' to '" + newName + "' because options already contains '" + newName + "'");

	if(!options.isMember(oldName))
		throw upgradeError("Could not rename option '" + oldName + "' to '" + newName + "' because options does not contain '" + oldName + "'");

	options[newName] = options[oldName];
	options.removeMember(oldName);

	msgs[newName] = msgs[oldName];
	msgs.erase(oldName);

	msgs[logId].push_back(prefixLog + "Renamed option '" + oldName + "' to '" + newName + "'");
}

void ChangeRename::setFrom(QString from)
{
	if (_from == from)
		return;

	_from = from;
	emit fromChanged(_from);
}

void ChangeRename::setTo(QString to)
{
	if (_to == to)
		return;

	_to = to;
	emit toChanged(_to);
}

}
