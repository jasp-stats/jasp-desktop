#ifndef UPGRADESTEP_H
#define UPGRADESTEP_H

#include "version.h"
#include "upgradechange.h"


/// This file is part of the older upgrades.json style of upgrading analyses. In the future Upgrades.qml should be used preferably

namespace Modules
{

class DynamicModule;

///Part of the monolithic upgrade process as defined by upgrades.json
class UpgradeStep
{
public:
	UpgradeStep(const Json::Value & upgradeEntry, const std::string & module);
	~UpgradeStep();

	void applyChanges(Json::Value & options, UpgradeMsgs & msgs) const;

	const Version		& fromVersion()		const { return _fromVersion;	}
	const Version		& toVersion()		const { return _toVersion;		}
	const std::string	& fromModule()		const { return _fromModule;		}
	const std::string	& toModule()		const { return _toModule;		}
	const std::string	& fromFunction()	const { return _fromFunction;	}
	const std::string	& toFunction()		const { return _toFunction;		}

	std::string toString() const;

private:
	Version							_fromVersion,
									_toVersion;
	std::string						_fromModule,
									_toModule,
									_fromFunction,
									_toFunction;
	std::vector<std::string>		_msgs;

	std::vector<UpgradeChange*>	_changes;
};

}

#endif // UPGRADESTEP_H
