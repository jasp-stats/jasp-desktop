#include "upgrader.h"
#include "log.h"
#include "analysis/analysis.h"
#include "../dynamicmodule.h"
#include <QFile>
#include "gui/messageforwarder.h"
#include <QTimer>

namespace Modules
{

Upgrader * Upgrader::_singleton = nullptr;

bool Upgrader::StepTaken::operator< (const StepTaken & other) const
{
	if(module <	other.module)	return true;
	if(module >	other.module)	return false;

	if(name < other.name)		return true;
	if(name > other.name)		return false;

	if(version < other.version) return true;

	return false;
}

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
}

void Upgrader::processUpgradeJson(const std::string & module, const Json::Value & upgrades, DynamicModule * dynMod)
{
	try
	{
		if(_allSteps.count(module) > 0)
			removeStepsOfModule(module);

		if(!upgrades.isArray())
			throw UpgradeStep::upgradeLoadError(upgrades, "Cannot load upgrades for module '" + module + "' because it is not an array of upgrade steps... This is what it looks like:");

		Steps & steps = _allSteps[module];

		for(const Json::Value & entry : upgrades)
			steps.push_back(new UpgradeStep(entry, module, dynMod));


		for(const UpgradeStep * step : steps)
			if(_searcher[step->fromModule()].count(step->fromVersion()) > 0 && _searcher[step->fromModule()][step->fromVersion()].count(step->fromFunction()) > 0)
				throw UpgradeStep::upgradeLoadError(upgrades, "Could not store step " + step->toString() + " because for this fromModule and fromVersion there is already a step stored: " + _searcher[step->fromModule()][step->fromVersion()][step->fromFunction()]->toString());
			else
				_searcher[step->fromModule()][step->fromVersion()][step->fromFunction()] = step;
	}
	catch(UpgradeStep::upgradeLoadError & e)
	{
		Log::log() << "Loading upgrades of '" << module << "'" << (dynMod ? " (dynamic mod: " + dynMod->name() + ")" : "") << " failed with error:\n" << e.what() << std::endl;
		MessageForwarder::showWarning(tr("Error Loading Upgrades"), tr("While loading upgrades for %1 an error was encountered: %2").arg(tq(dynMod ? "module '" + dynMod->name() + "'" : "JASP")).arg(e.what()));
	}
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
			throw UpgradeStep::upgradeLoadError(Json::nullValue, "Could not remove step " + step->toString() + " of module '" + module + "' because it isn't stored in the searcher, are you removing multiple times?");
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

bool Upgrader::upgradeAnalysisData(Json::Value & analysis, UpgradeMsgs & msgs) const
{
	StepsTaken	stepsTaken;

	try
	{
		_upgradeOptionsFromJaspFile(analysis, msgs, stepsTaken);
	}
	catch(UpgradeChange::upgradeError & e)
	{
		Log::log() << "Applying upgrades to analysis failed with error:\n" << e.what() << std::endl;
		msgs[""].push_back(std::string("Failed to apply analysis upgrade with error: ") + e.what());

		MessageForwarder::showWarning(tq("Analysis Upgrade Failed"), tq("Upgrading analysis failed with error: %1").arg(e.what()));
	}

	return stepsTaken.size() > 0;
}

void Upgrader::_upgradeOptionsFromJaspFile(Json::Value & analysis, UpgradeMsgs & msgs, StepsTaken & stepsTaken) const
{
	std::string		module		= analysis.get("module", "Common").asString(), //analysis.isMember("dynamicModule") ? analysis["dynamicModule"][
					function	= analysis["name"].asString(); //name in a jasp file analyses.json refers to the function...
	Version			version		= analysis["version"].asString();
	Json::Value &	options		= analysis["options"];

	//Ok apparently some old JASP files have version-numbers like "1.0" in 0.8.2, which is not good.. So let's check if module was filled and version is that, in that case we treat it as 0
	if(!analysis.isMember("module") && version == Version(1))
		version = Version(0, 8, 2);

	Log::log() << "Checking if there are upgrade options for module '" << module << "' with function '" << function << "' and version '" << version.toString() << "'!" << std::endl;

	if(_searcher.count(module) == 0)
	{
		Log::log() << "No such upgrade options were found, keeping it as is." << std::endl;
		return;
	}

	const StepsPerVersion & perVersion = _searcher.at(module);

	Version closestVersion = version;
	if(perVersion.count(version) == 0 || perVersion.at(version).count(function) == 0) //If this specific version has no upgrades then maybe a later version does!
		for(auto & versionSteps : perVersion)
			if(versionSteps.first > version && versionSteps.second.count(function) > 0)
			{
				closestVersion = versionSteps.first;
				break;
			}

	if(perVersion.count(closestVersion) > 0 && perVersion.at(closestVersion).count(function) > 0) //apply step!
	{
		Log::log() << "Closest (from) version found was: '" << closestVersion.toString() << "'" << std::endl;

		const UpgradeStep * step = perVersion.at(closestVersion).at(function);

		//Do some loop detection
		StepTaken	fromStep(		{module,			function,			closestVersion}),
					aboutToStep(	{step->toModule(),	step->toFunction(), step->toVersion()});

		stepsTaken.insert(fromStep); //We want to remember where we come from

		if(stepsTaken.count(aboutToStep) > 0)
			throw UpgradeChange::upgradeError("Aborting upgrade because a loop was detected!\n\nIf " + step->toString() + " is taken, eventually it is reached again.\n\nThis should definitely not happen, perhaps the module author of '" + module + "' can be of assistance");

		step->applyChanges(options, msgs);
		stepsTaken.insert(aboutToStep); //And remember where we are going

		if(step->dynamicModule())
			throw std::runtime_error("You should probably implement this! see void Upgrader::_upgradeOptionsFromJaspFile(Json::Value & analysis, UpgradeMsgs & msgs, StepsTaken & stepsTaken) const");
			//analysis["dynamicModule"] = step->dynamicModule()->asJsonForJaspFile();

		analysis["module"]	= step->toModule();
		analysis["version"] = step->toVersion().toString();
		analysis["name"]	= step->toFunction();

		for(const std::string & optionLog : msgs[logId])
			Log::log() << optionLog << std::endl;
		msgs[logId].clear();

		Log::log() << "Options were upgraded to module '" << step->toModule() << "' with function '" << step->toFunction() << "' and version '" << step->toVersion().toString() << "'!" << std::endl;

		_upgradeOptionsFromJaspFile(analysis, msgs, stepsTaken); //See if we can upgrade some more
	}
	else
		Log::log () << "Nope, no upgrades to be done." << std::endl;
}

void Upgrader::loadOldSchoolUpgrades()
{
	QFile upgradesFile(":/resources/../modules/upgrader/upgrades.json");
	upgradesFile.open(QFile::OpenModeFlag::Text | QFile::OpenModeFlag::ReadOnly);

	if(upgradesFile.isOpen())
	{

		std::string upgradesTxt = upgradesFile.readAll().toStdString();
		Json::Value upgradesJson;

		if(Json::Reader().parse(upgradesTxt, upgradesJson))
		{
			processUpgradeJson("JASP", upgradesJson);
			return;
		}
	}

	MessageForwarder::showWarning(tr("Upgrades couldn't be read"),
		tr("The necessary upgrades for reading older (<0.12) JASP-files could not be read...\nYou can still use JASP and even read (some) older files but some options might not be understood properly and some analyses might fail to load entirely."));
}

}
