#include "installedmodules.h"
#include "utilities/appdirs.h"
#include <QString>
#include <QDir>
#include <json/json.h>
#include <fstream>
#include "gui/preferencesmodel.h"
#include "log.h"
#include "resultstesting/compareresults.h"


const std::string InstalledModules::settingsPath = "modules-settings.json";

void InstalledModules::parseModuleInfo(const std::string& path, InstalledModules::ModuleInfo& info) {
	InstalledModules::ModuleInfo module;

	std::ifstream in(path);
	Json::Value root;
	Json::Reader reader;
	bool parseOk = reader.parse(in, root);
	
	if(!parseOk)
		throw std::runtime_error("Failed to parse manifest JSON: " + reader.getFormattedErrorMessages());

	std::string strVersion = "";
	if(root.isMember("version")) {
		if(root["version"].isString()) {
			strVersion = root["version"].asString();
		}
		else if(root["version"].isObject() && root["version"].isMember("Version")) {
			strVersion = root["version"]["Version"].asString();
		}
		else {
			Json::Value version = root["version"][0];
			for(int i = 0; i < version.size(); i++) {
				auto x = version[i];
				strVersion =  strVersion + version[i].asString() + ".";
			}
			strVersion.pop_back();
		}
	}

	info.name = root["name"].asString();
	info.version = BundleVersion(strVersion);
}

std::vector<InstalledModules::ModuleInfo> InstalledModules::getAllAvailableModules() {
	std::vector<InstalledModules::ModuleInfo> modules = {};

	auto parseManifests = [&](const std::string& path, bool bundled = false) {
		auto dir = QDir(path.c_str());
		dir.cdUp(); dir.cd("manifests");
		if(!dir.exists())
			return;

		auto manifests = dir.entryList({"jasp*.json"}, QDir::Files);
		for(auto& manifest : manifests) {
			InstalledModules::ModuleInfo info;
			try {
				parseModuleInfo(dir.filePath(manifest).toStdString(), info);
			}
			catch(std::exception & e) { 
				Log::log() << "Could not parse module manifest, error: " << e.what() << std::endl;
				continue; 
			}
			info.libpath = QDir(path.c_str()).filePath(info.name.c_str()).toStdString();
			info.bundled = bundled;
			modules.push_back(info);
		}
	};

	parseManifests(AppDirs::bundledModulesLibDir().toStdString(), true);
	parseManifests(AppDirs::userModulesLibDir().toStdString());
	return modules;
}


std::vector<InstalledModules::ModuleInfo> InstalledModules::getModules() {

	auto modulesAll = getAllAvailableModules();
	std::map<std::string, InstalledModules::ModuleInfo> modules;
	for(const auto& module : modulesAll) { //remove duplicates take highest version
		if(modules.find(module.name) == modules.end() || modules[module.name] < module) {
			modules[module.name] = module;
		}
	}

	if(!PreferencesModel::prefs()->developerMode() && !resultXmlCompare::compareResults::theOne()->testMode())
		modules.erase("jaspTestModule");


	//get the module orders and groupings form the modules settings file
	//This will probably be replaced with org specific settings from toml file in future
	std::string settings = AppDirs::bundledModulesDir().toStdString() + settingsPath;
	std::ifstream in(settings);
	Json::Value root;
	Json::Reader().parse(in, root);
	Json::Value commonNamesJson = root.get("common", Json::arrayValue),
				extraNamesJson	= root.get("extra", Json::arrayValue);
	if(!commonNamesJson.isArray())	commonNamesJson = Json::arrayValue;
	if(!extraNamesJson.isArray())	extraNamesJson = Json::arrayValue;


	std::vector<InstalledModules::ModuleInfo> orderedModules = {};
	for(auto & name : commonNamesJson) {
		if(modules.find(name.asString()) != modules.end()) {
			modules[name.asString()].common = true;
			orderedModules.push_back(modules[name.asString()]);
			modules.erase(name.asString());
		}
	}
	for(auto & name : extraNamesJson) {
		if(modules.find(name.asString()) != modules.end()) {
			orderedModules.push_back(modules[name.asString()]);
			modules.erase(name.asString());
		}
	}

	//insert leftover modules
	for(auto& module : modules)
		orderedModules.push_back(module.second);

	return orderedModules;
}



std::map<std::string, std::string> InstalledModules::getInstalledModuleVersions()
{
	std::map<std::string, std::string> moduleVersionMap;
	auto modules = getModules();
	for(auto& module : modules) {
		moduleVersionMap[module.name] = module.version.asString(3);
	}
	return moduleVersionMap;
}


