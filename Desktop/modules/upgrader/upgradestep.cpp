#include "upgradestep.h"
#include <sstream>

namespace Modules
{

UpgradeStep::UpgradeStep(const Json::Value & upgradeEntry, const std::string & module) : _toModule(module)
{
	if(!upgradeEntry.isMember("from"))	throw upgradeLoadError(upgradeEntry, "UpgradeStep cannot be loaded because 'from' is missing from:");
	if(!upgradeEntry.isMember("to"))	throw upgradeLoadError(upgradeEntry, "UpgradeStep cannot be loaded because 'to' is missing from:");
	//options does not necessarily need to be there, maybe the analysis was just moved to another module

	{
		const Json::Value	& from	= upgradeEntry["from"],
							& to	= upgradeEntry["to"];

		if(!from.isMember("version"))	throw upgradeLoadError(from,	"UpgradeStep cannot be loaded because 'version'  is missing from 'from':");
		if(!from.isMember("function"))	throw upgradeLoadError(from,	"UpgradeStep cannot be loaded because 'function' is missing from 'from':");
		if(!to.isMember("version"))		throw upgradeLoadError(to,		"UpgradeStep cannot be loaded because 'version'  is missing from 'to':");

		try									{	_fromVersion = from["version"].asString(); }
		catch(Version::encodingError & e)	{ throw upgradeLoadError(from, std::string("Wrong fromVersion '") + e.what() + "'"); }

		try									{	_toVersion = to["version"].asString(); }
		catch(Version::encodingError & e)	{ throw upgradeLoadError(to, std::string("Wrong toVersion '") + e.what() + "'"); }

		_fromFunction	= from["function"].asString();
		_fromModule		= from.get("module", module).asString();

		_toFunction		= _fromFunction == "*" ? "*" : to.get("function", _fromFunction).asString();
		_toModule		= to.get("module", module).asString();
	}

	if(_fromFunction != "") //If we don't specify the particular function it is a general module renaming!
		for(const Json::Value & change : upgradeEntry.get("options", Json::arrayValue))
			_changes.push_back(new UpgradeChange(change));

	if(upgradeEntry.isMember("msg") && upgradeEntry["msg"].isString())
		_msgs.push_back(upgradeEntry["msg"].asString());
}

UpgradeStep::~UpgradeStep()
{
	for(UpgradeChange * change : _changes)
		delete change;
	_changes.clear();
}

std::string UpgradeStep::toString() const
{
	std::stringstream out;

	bool	sameModule	= _fromModule	== _toModule,
			sameName	= _fromFunction		== _toFunction;

	out << "UpgradeStep ( "
		<< (sameModule	? "for module "	+ _fromModule	: "where module " + _fromModule + " -> " + _toModule)
		<< ", "
		<< (sameName	? "for analysis "	+ _fromFunction		: "where analysis " + _fromFunction + " to " + _toFunction)
		<< ", for version " << _fromVersion.asString() << " -> " << _toVersion.asString()
		<< " and with #" << _changes.size() << " optionchanges)";

	return out.str();
}


void UpgradeStep::applyChanges(Json::Value & options, UpgradeMsgs & msgs) const
{
	for(UpgradeChange * change : _changes)
		change->applyUpgrade(options, msgs);

	msgs[""].insert(msgs[""].begin(), _msgs.begin(), _msgs.end());
}

}
