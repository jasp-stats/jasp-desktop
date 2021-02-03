#include "utilities/qutils.h"
#include "upgrades.h"
#include "upgrade.h"
#include "log.h"

namespace Modules
{

Upgrades::Upgrades()
{

}

bool Upgrades::findClosestVersion(const std::string & function, Version & version)
{

	Version closestVersion = version;

	if(		_steps.count(version)				== 0	||	//There is nothing registered for this version
			_steps.at(version).count(function)	== 0	||	//Or there is nothing registered for this version + function
			_steps.at(version).count("*")		== 0	)	//Or there is nothing registered for this version and module (function == "*" which means all functions)
		for(auto & versionSteps : _steps)
			if(versionSteps.first > version && (versionSteps.second.count(function) > 0 || versionSteps.second.count("*") > 0)) //This works because _steps is ordered on Version and we can thus assume each version is > than the last
			{
				closestVersion = versionSteps.first;
				break;
			}

	if(_steps.count(closestVersion) > 0 && (_steps.at(closestVersion).count(function) > 0 || _steps.at(closestVersion).count("*") > 0)) //apply step!
	{
		Log::log() << "Closest (from) version found was: '" << closestVersion.asString() << "'" << std::endl;
		version = closestVersion;
		return true;
	}

	return false;
}


void Upgrades::applyUpgrade(const std::string & function, const Version & version, Json::Value & analysesJson, UpgradeMsgs & msgs, StepsTaken & stepsTaken)
{
	Version closest = version;

	if(findClosestVersion(function, closest))
		_steps[closest][function]->applyUpgrade(function, version, analysesJson, msgs, stepsTaken);
}

void Upgrades::setModule(QString module)
{
	if (_module == module)
		return;
	
	_module = module;
	emit moduleChanged(_module);
}

void Upgrades::addStep(Upgrade * step)
{
	if(_steps.count(step->fromVersion()) > 0 && _steps[step->fromVersion()].count(fq(step->functionName())) > 0 && _steps[step->fromVersion()][fq(step->functionName())]  != step)
		throw upgradeLoadError(fq(module()), fq("Already registered an Upgrade with version '" + step->fromVersionQ() + "'"));
	
	Log::log() << "Registering Upgrade '" << step->toString() << "' for module '" << module() << "'." << std::endl;
	
	_steps[step->fromVersion()][fq(step->functionName())] = step;
}

void Upgrades::removeStep(Upgrade * step)
{
	if(_steps.count(step->fromVersion()) > 0 && _steps[step->fromVersion()].count(fq(step->functionName())) > 0)
	{
		Log::log() << "Removing Upgrade '" << step->toString() << "' from module '" << module() << "'." << std::endl;
		
		_steps[step->fromVersion()].erase(fq(step->functionName()));	
		_steps.erase(step->fromVersion());
	}
}

}
