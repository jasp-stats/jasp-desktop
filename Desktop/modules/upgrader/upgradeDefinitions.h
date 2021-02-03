#ifndef UPGRADEDEFINITIONS_H
#define UPGRADEDEFINITIONS_H

#include <set>
#include <map>
#include <vector>
#include <string>
#include "version.h"
#include "jsonredirect.h"


namespace Modules
{
typedef std::map<std::string, std::vector<std::string>> UpgradeMsgs; //option name -> list msgs. "" as option means entire analysis/form

extern const std::string		prefixLog;
extern const char			*	logId;

struct upgradeError  : public std::runtime_error
{
	upgradeError(std::string msg) : std::runtime_error(msg) {}
	const char* what() const noexcept override;
};

struct upgradeLoadError  : public std::runtime_error
{
	upgradeLoadError(const Json::Value & currentJson, std::string msg) : std::runtime_error(msg + "\n'" + currentJson.toStyledString() + "'") {}
	const char* what() const noexcept override;
};


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
