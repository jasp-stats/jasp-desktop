#include "upgrader.h"
#include "log.h"
#include "qutils.h"
#include <QFile>
#include "utilities/messageforwarder.h"
#include <QTimer>

namespace Modules
{

Upgrader * Upgrader::_singleton = nullptr;



Upgrader::Upgrader(QObject *parent) : QObject(parent)
{
	if(_singleton) throw std::runtime_error("Upgrader should only be instantiated once.");
	_singleton = this;
}

Upgrader::~Upgrader()
{
	_searcher.clear();

	for(auto & modSteps : _allSteps)
		for(UpgradeStep * step : modSteps.second)
			delete step;

	_allSteps.clear();

	_singleton = nullptr;
}

void Upgrader::removeStepsOfModule(const std::string & module)
{
	if(_allSteps.count(module) == 0)
	{
		Log::log() << "Could not removes steps for module '" + module + "' because there were none." << std::endl;
		return;
	}

	Steps & steps = _allSteps[module];

	for(const UpgradeStep * step : steps)
		if(_searcher[step->fromModule()].count(step->fromVersion()) == 0 || _searcher[step->fromModule()][step->fromVersion()].count(step->fromFunction()) == 0)
			throw upgradeLoadError(Json::nullValue, "Could not remove step " + step->toString() + " of module '" + module + "' because it isn't stored in the searcher, are you removing multiple times?");
		else
		{
			_searcher[step->fromModule()][step->fromVersion()].erase(step->fromFunction());

			if(_searcher[step->fromModule()][step->fromVersion()].size() == 0)
				_searcher[step->fromModule()].erase(step->fromVersion());
		}

	for(const UpgradeStep * step : steps)
		delete step;

	_allSteps.erase(module);
}

bool Upgrader::upgradeAnalysisData(const ModulesMap& modules, Json::Value & analysis, UpgradeMsgs & msgs) const
{
	StepsTaken	stepsTaken;

	analysis["preUpgradeVersion"] = analysis["version"];

	try
	{
		_upgradeOptionsFromJaspFile(modules, analysis, msgs, stepsTaken);
		
		

#ifdef JASP_DEBUG
		Log::log() << "Options are now: " << analysis["options"].toStyledString() << std::endl;
#endif

	}
	catch(upgradeError & e)
	{
		Log::log() << "Applying upgrades to analysis failed with error:\n" << e.what() << std::endl;
		
		msgs[""].push_back(std::string("Failed to apply analysis upgrade with error: ") + e.what());

		MessageForwarder::showWarning(tq("Analysis Upgrade Failed"), tq("Upgrading analysis failed with error: %1").arg(e.what()));
	}

	return stepsTaken.size() > 0;
}

void Upgrader::_upgradeOptionsFromJaspFile(const ModulesMap & modules, Json::Value & analysis, UpgradeMsgs & msgs, StepsTaken & stepsTaken) const
{
	std::string		module		= (analysis.isMember("dynamicModule") ? analysis["dynamicModule"]["moduleName"]		: analysis.get("module", "Common")	).asString(),
					function	= (analysis.isMember("dynamicModule") ? analysis["dynamicModule"]["analysisEntry"]	: analysis["name"]					).asString(); //name in a jasp file analyses.json refers to the function... analysis["name"] really should be the same as in ...["analysisEntry"] btw. Left the ternary here cause it looks nicer
	Version			version		= (analysis.isMember("dynamicModule") ? analysis["dynamicModule"]["moduleVersion"]	: analysis["version"]				).asString();
	Json::Value &	options		= analysis["options"];

	//Ok apparently some old JASP files have version-numbers like "1.0" in 0.8.2, which is not good.. So let's check if module was filled and version is that, in that case we treat it as 0
	if(!analysis.isMember("module") && version == Version(1))
		version = Version(0, 8, 2);

	Log::log() << "Checking if there are upgrade options for module '" << module << "' with function '" << function << "' and version '" << version.asString() << "'!" << std::endl;

	if(_searcher.count(module) > 0)
	{
		const StepsPerVersion & perVersion = _searcher.at(module);
	
		Version closestVersion = version;
		if(		perVersion.count(version)				== 0	||	//There is nothing registered for this version
				perVersion.at(version).count(function)	== 0	||	//Or there is nothing registered for this version + function
				perVersion.at(version).count("*")		== 0	)	//Or there is nothing registered for this version and module (function == "*" which means all functions)
			for(auto & versionSteps : perVersion)
				if(versionSteps.first > version && (versionSteps.second.count(function) > 0 || versionSteps.second.count("*") > 0))
				{
					closestVersion = versionSteps.first;
					break;
				}
	
		if(perVersion.count(closestVersion) > 0 && (perVersion.at(closestVersion).count(function) > 0 || perVersion.at(closestVersion).count("*") > 0)) //apply step!
		{
			Log::log() << "Closest (from) version found was: '" << closestVersion.asString() << "'" << std::endl;
	
			const UpgradeStep * step = perVersion.at(closestVersion).count(function) > 0 ? perVersion.at(closestVersion).at(function) : perVersion.at(closestVersion).at("*");
	
			const std::string toFunction = step->toFunction() == "*" ? function : step->toFunction();
			//Do some loop detection
			StepTaken	fromStep(		{module,			function,	closestVersion}),
						aboutToStep(	{step->toModule(),	toFunction,	step->toVersion()});
	
			stepsTaken.insert(fromStep); //We want to remember where we come from
	
			if(stepsTaken.count(aboutToStep) > 0)
				throw upgradeError("Aborting upgrade because a loop was detected!\n\nIf " + step->toString() + " is taken, eventually it is reached again.\n\nThis should definitely not happen, perhaps the module author of '" + module + "' can be of assistance");
	
			step->applyChanges(options, msgs);
			stepsTaken.insert(aboutToStep); //And remember where we are going
			
			
			if(analysis.isMember("module"))
			{
				analysis["module"]	= step->toModule();
				analysis["version"] = step->toVersion().asString();
				analysis["name"]	= toFunction;
			}
			else //It must have already been in a "dynamicModule" sub entry
			{
				analysis["dynamicModule"]["moduleName"]		= step->toModule();
				analysis["dynamicModule"]["moduleVersion"]	= step->toVersion().asString();
				analysis["dynamicModule"]["analysisEntry"]	= toFunction;
				
			}
				
			for(const std::string & optionLog : msgs[logId])
				Log::log() << optionLog << std::endl;
			msgs[logId].clear();
	
			Log::log() << "Options were upgraded to module '" << step->toModule() << "' with function '" << step->toFunction() << "' and version '" << step->toVersion().asString() << "'!" << std::endl;
	
			_upgradeOptionsFromJaspFile(modules, analysis, msgs, stepsTaken); //See if we can upgrade some more
			
			return;
		}
	}
	
	if (modules.count(module)> 0 && modules.at(module)->hasUpgradesToApply(function, version))
	{
		modules.at(module)->applyUpgrade(function, version, analysis, msgs, stepsTaken);
		_upgradeOptionsFromJaspFile(modules, analysis, msgs, stepsTaken);
	}
	else
		Log::log () << "Nope, no upgrades to be done." << std::endl;
}

}
