#include "changecopy.h"
#include "utilities/qutils.h"

namespace Modules
{


ChangeCopy::ChangeCopy()
{

}

void ChangeCopy::applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const
{
	//It is ok to overwrite whatever is in newName? Because it is a sort of "set value"

	const std::string	oldName = fq(_from),
						newName = fq(_to);

	if(!options.isMember(oldName))
		throw upgradeError("Could not copy option '" + oldName + "' to '" + newName + "' because options does not contain '" + oldName + "'");

	options[newName]	= options[oldName];
	msgs[newName]		= msgs[oldName];

	msgs[logId].push_back(prefixLog + "Copied option '" + oldName + "' to '" + newName + "'");

	if(msg() != "")
		msgs[newName].push_back(fq(msg()));
}

void ChangeCopy::setFrom(QString from)
{
	if (_from == from)
		return;

	_from = from;
	emit fromChanged(_from);
}

void ChangeCopy::setTo(QString to)
{
	if (_to == to)
		return;

	_to = to;
	emit toChanged(_to);
}

}
