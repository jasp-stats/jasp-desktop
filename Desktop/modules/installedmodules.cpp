#include "installedmodules.h"
#include "utilities/appdirs.h"
#include <QString>
#include <QDir>
#include <json/json.h>
#include <fstream>
#include "gui/preferencesmodel.h"
#include "log.h"


const std::string InstalledModules::settingsPath = "modules-settings.json";


QStringList getModulesFromDir(const std::string& path)
{
	auto dir = QDir(path.c_str());
	if(!dir.exists())
		return {};

	return dir.entryList({"jasp*"}, QDir::Dirs);
}

std::vector<std::string> InstalledModules::getModules(bool extra) {
	//get the module orders and groupings form the modules settings file
	//This will probably be replaced with org specific settings from toml file in future
	std::string settings = AppDirs::bundledModulesDir().toStdString() + settingsPath;
	std::ifstream in(settings);
	Json::Value root;
	Json::Reader().parse(in, root);
	Json::Value commonNames = root.get("common", Json::arrayValue);
	Json::Value extraNames = root.get("extra", Json::arrayValue);
	if(!commonNames.isArray()) commonNames = Json::arrayValue;
	if(!extraNames.isArray()) extraNames = Json::arrayValue;


	//collect all available modules
    QStringList availableModules = getModulesFromDir(AppDirs::bundledModulesLibDir().toStdString());
    QStringList installedModules = getModulesFromDir(AppDirs::userModulesLibDir().toStdString());
    availableModules.append(installedModules);
    availableModules.removeDuplicates();

    if(!PreferencesModel::prefs()->developerMode())
        availableModules.removeAll("jaspTestModule");

	//separate into groups ordered by listing in settings
	//ugly nˆ2 ish but n < 100 so fine
	std::vector<std::string> extraModules;
	for(auto& name: extraNames)
		if(availableModules.contains(name.asCString())) {
			extraModules.push_back(name.asCString());
			availableModules.removeAll(name.asCString());
		}
	if(extra) return extraModules;

	std::vector<std::string> commonModules;
	for(auto& name: commonNames)
		if(availableModules.contains(name.asCString())) {
			commonModules.push_back(name.asCString());
			availableModules.removeAll(name.asCString());
		}
	for(auto& module : availableModules) commonModules.push_back(module.toStdString());
	return commonModules;
}

std::vector<std::string> InstalledModules::getActiveCommonModules()
{
	return getModules();
}

std::vector<std::string> InstalledModules::getActiveExtraModules()
{
	return getModules(true);
}


std::map<std::string, std::string> InstalledModules::getInstalledModuleVersions()
{
	std::map<std::string, std::string> moduleVersionMap;

	auto parseManifests = [&](const std::string& path) {
		auto dir = QDir(path.c_str());
		dir.cdUp(); dir.cd("manifests");
		if(!dir.exists())
			return;

		auto manifests = dir.entryList({"jasp*.json"}, QDir::Files);
		for(auto& manifest : manifests) {
			std::ifstream in(dir.absoluteFilePath(manifest).toStdString());
			Json::Value root;
			Json::Reader().parse(in, root);
			Json::Value version = root["version"][0];
			std::string strVersion = "";
			for(int i = 0; i < version.size(); i++) {
				auto x = version[i];
				strVersion =  strVersion + version[i].asString() + ".";
			}
			strVersion.pop_back();
			moduleVersionMap[root["name"].asString()] = strVersion;
		}

	};

	parseManifests(AppDirs::bundledModulesLibDir().toStdString());
	parseManifests(AppDirs::userModulesLibDir().toStdString());

	return moduleVersionMap;
}


