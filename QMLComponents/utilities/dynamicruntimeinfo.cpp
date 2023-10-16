#include "dynamicruntimeinfo.h"
#include "json/json.h"
#include "appinfo.h"
#include "appdirs.h"
#include "log.h"
#include "utilities/qutils.h"

#include <fstream>
#include <chrono>


DynamicRuntimeInfo* DynamicRuntimeInfo::_instance = nullptr;
const std::string DynamicRuntimeInfo::staticInfoFileName = "staticRuntimeInfo.json";
const std::string DynamicRuntimeInfo::dynamicInfoFileName = "dynamicRuntimeInfo.json";

DynamicRuntimeInfo *DynamicRuntimeInfo::getInstance()
{
    if(!_instance)
        _instance = new DynamicRuntimeInfo();
    return _instance;
}

bool DynamicRuntimeInfo::bundledModulesInitialized()
{
	bool res = true;

	if(_environment == RuntimeEnvironment::MSI || _environment == RuntimeEnvironment::MSIX || _environment == RuntimeEnvironment::ZIP)
		res = _bundledModulesInitializedSet
			  && _initializedByCommit == AppInfo::gitCommit
			  && _initializedByBuildDate == AppInfo::builddate
			  && _initializedForRVersion == AppInfo::getRVersion();

	return res;
}

DynamicRuntimeInfo::DynamicRuntimeInfo()
{
#ifdef FLATPAK_USED
	_environment = RuntimeEnvironment::FLATPAK;
#elif __APPLE__
	_environment = RuntimeEnvironment::MAC;
#elif linux
	_environment = RuntimeEnvironment::LINUX_LOCAL;
#elif _WIN32
	parseStaticRuntimeInfoFile(fq(AppDirs::programDir().absoluteFilePath(tq(staticInfoFileName)))); //will set runtime env info
	parseDynamicRuntimeInfoFile(fq(AppDirs::appData(false) + "/" + tq(dynamicInfoFileName)));
#else
    _environment = RuntimeEnvironment::UNKNOWN;
#endif


#if defined(__aarch64__)
    _arch = MicroArch::AARCH64;
#elif defined(__x86_64__) || defined(_M_X64)
    _arch = MicroArch::X86_64;
#else
    _arch = MicroArch::UNSUPPORTED;
#endif
}

bool DynamicRuntimeInfo::parseStaticRuntimeInfoFile(const std::string &path)
{
	Log::log() << "Attempting to read static runtime information from: " + path << std::endl;
	std::ifstream in(path, std::ifstream::in);
    if(!in)
    {
        _environment = RuntimeEnvironment::UNKNOWN;
        Log::log() << "Failed to open specified static runtime file" << std::endl;
        return false;
    }

    Json::Value root;
    in >> root;

	std::string runtimeEnvironmentString = root.get("runtimeEnvironment", "").asString();
	auto it = StringToRuntimeEnvironmentMap.find(runtimeEnvironmentString);
	if(it == StringToRuntimeEnvironmentMap.end())
        _environment = RuntimeEnvironment::UNKNOWN;
    else 
        _environment = it->second;

    return true;
}

bool DynamicRuntimeInfo::parseDynamicRuntimeInfoFile(const std::string &path)
{
	Log::log() << "Attempting to read dynamic runtime information from: " + path << std::endl;
    std::ifstream in(path, std::ifstream::in);
    if(!in)
	{
		_bundledModulesInitializedSet = false;
		Log::log() << "Failed to open specified dynamic runtime file or it may simply not exist yes" << std::endl;
        return false;
    }

    Json::Value root;
    in >> root;

	_bundledModulesInitializedSet = root.get("initialized", false).asBool();
    _initializedByCommit = root.get("commit", "").asString();
	_initializedByBuildDate = root.get("buildDate", "").asString();
    _initializedForRVersion = root.get("RVersion", "").asString();
	_initializedOn = root.get("initTimestamp", 0).asUInt64();

    return true;

}

bool DynamicRuntimeInfo::writeDynamicRuntimeInfoFile()
{
	std::string path = fq(AppDirs::appData(false) + "/" + tq(dynamicInfoFileName));
    std::ofstream out(path, std::ofstream::out);
    if(!out)
    {
		Log::log() << "Failed to open specified path for writing dynamic runtime info file" << std::endl;
        return false;
    }

    Json::Value root;
    root["initialized"] = true;
	root["commit"] = AppInfo::gitCommit;
	root["buildDate"] = AppInfo::builddate;
	root["RVersion"] = AppInfo::getRVersion();
	uint64_t timestamp = std::chrono::system_clock::now().time_since_epoch() / std::chrono::milliseconds(1);
	root["initTimestamp"] = timestamp;

	out << root;
	return true;
}

DynamicRuntimeInfo::RuntimeEnvironment DynamicRuntimeInfo::getRuntimeEnvironment()
{
	return _environment;
}

std::string DynamicRuntimeInfo::getRuntimeEnvironmentAsString()
{
	auto x =  RuntimeEnvironmentToStringMap.find(_environment);
	return x != RuntimeEnvironmentToStringMap.end() ? x->second : RuntimeEnvironmentToStringMap.find(UNKNOWN)->second;
}

uint64_t DynamicRuntimeInfo::bundledModulesInitializedOnTimestamp()
{
    return _initializedOn;
}

std::string DynamicRuntimeInfo::bundledModulesInitializedByCommit()
{
	return _initializedByCommit;
}

std::string DynamicRuntimeInfo::bundledModulesInitializedByBuildDate()
{
	return _initializedByBuildDate;
}

std::string DynamicRuntimeInfo::bundledModulesInitializedRVersion()
{
	return _initializedForRVersion;
}
