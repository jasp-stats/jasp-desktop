#ifndef UPGRADESTEP_H
#define UPGRADESTEP_H

#include "modules/upgrader/version.h"
#include "upgradechange.h"


namespace Modules
{

class DynamicModule;

class UpgradeStep
{
public:
	struct upgradeLoadError  : public std::runtime_error
	{
		upgradeLoadError(const Json::Value & currentJson, std::string msg) : std::runtime_error(msg + "\n'" + currentJson.toStyledString() + "'") {}
		const char* what() const noexcept override;
	};


	UpgradeStep(const Json::Value & upgradeEntry, const std::string & module, DynamicModule * dynamicModule = nullptr);
	~UpgradeStep();

	void applyChanges(Json::Value & options, UpgradeMsgs & msgs) const;

	const Version		& fromVersion()		const { return _fromVersion;	}
	const Version		& toVersion()		const { return _toVersion;		}
	const std::string	& fromModule()		const { return _fromModule;		}
	const std::string	& toModule()		const { return _toModule;		}
	const std::string	& fromFunction()	const { return _fromFunction;	}
	const std::string	& toFunction()		const { return _toFunction;		}

	const DynamicModule * dynamicModule()	const { return _dynamicModule;	}

	std::string toString() const;

private:
	Version							_fromVersion,
									_toVersion;
	std::string						_fromModule,
									_toModule,
									_fromFunction,
									_toFunction;
	const DynamicModule			*	_dynamicModule;
	std::vector<std::string>		_msgs;

	std::vector<UpgradeChange*>	_changes;
};

}

#endif // UPGRADESTEP_H
