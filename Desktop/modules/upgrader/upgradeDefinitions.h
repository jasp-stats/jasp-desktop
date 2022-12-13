#ifndef UPGRADEDEFINITIONS_H
#define UPGRADEDEFINITIONS_H

#include <set>
#include <map>
#include <vector>
#include <string>
#include "version.h"
#include <json/json.h>


namespace Modules
{
typedef std::map<std::string, std::vector<std::string>> UpgradeMsgs; //option name -> list msgs. "" as option means entire analysis/form

extern const std::string		prefixLog;
extern const char			*	logId;

struct upgradeError  : public std::runtime_error
{
	bool isWarning = false;
	upgradeError(std::string msg, bool _isWarning = false) : std::runtime_error(msg) { isWarning = _isWarning; }
	const char* what() const noexcept override;
};


struct upgradeLoadError  : public std::runtime_error
{
	upgradeLoadError(const Json::Value & currentJson, std::string msg) : std::runtime_error(msg + "\n'" + currentJson.toStyledString() + "'") {}
	const char* what() const noexcept override;
};

/// Used to avoid loops by tracking each step and making sure they keep increasing in version.
/// And not recur, which would lead to infinity
struct StepTaken
{
	std::string module,
				name;
	Version		version;
	bool		operator<(const StepTaken & other) const;
};
typedef std::set<StepTaken> StepsTaken;

}

#endif // UPGRADEDEFINITIONS_H
