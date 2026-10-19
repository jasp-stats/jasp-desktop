//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
#include "dynamicruntimeinfo.h"
#include "appinfo.h"
#include "appdirs.h"
#include "log.h"
#include "qutils.h"
#include <fstream>
#include <chrono>
#include "dirs.h"


DynamicRuntimeInfo* DynamicRuntimeInfo::_instance			= nullptr;
const std::string DynamicRuntimeInfo::staticInfoFileName	= "staticRuntimeInfo.json";
const std::string DynamicRuntimeInfo::dynamicInfoFileName	= "dynamicRuntimeInfo.json";

DynamicRuntimeInfo *DynamicRuntimeInfo::getInstance()
{
    if(!_instance)
        _instance = new DynamicRuntimeInfo();

    return _instance;
}

bool DynamicRuntimeInfo::bundledModulesInitialized()
{
	bool res = true;

	RuntimeEnvironment environment = getRuntimeEnvironment();
	if(environment == RuntimeEnvironment::MSI || environment == RuntimeEnvironment::MSIX || environment == RuntimeEnvironment::ZIP)
		res = _bundledModulesInitializedSet
			  && _initializedByCommit		== AppInfo::gitCommit
			  && _initializedByBuildDate	== AppInfo::builddate
			  && _initializedForRVersion	== AppInfo::getRVersion()
			  && _initializedForJaspVersion == AppInfo::version.asString(4);

	return res;
}

MicroArch DynamicRuntimeInfo::getMicroArch()
{
#if defined(__aarch64__)
	return MicroArch::AARCH64;
#elif defined(__x86_64__) || defined(_M_X64)
	return MicroArch::X86_64;
#else
	return MicroArch::UNSUPPORTED;
#endif
}

DynamicRuntimeInfo::DynamicRuntimeInfo()
{
#ifdef _WIN32
	parseDynamicRuntimeInfoFile(dynamicRuntimeInfoFilePath());
#endif
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

	_bundledModulesInitializedSet	= root.get("initialized",	false).asBool();
	_initializedByCommit			= root.get("commit",		"").asString();
	_initializedByBuildDate			= root.get("buildDate",		"").asString();
	_initializedForRVersion			= root.get("RVersion",		"").asString();
	_initializedForJaspVersion		= root.get("jaspVersion",	"").asString();
	_initializedOn					= root.get("initTimestamp", 0).asUInt64();

    return true;

}

std::string DynamicRuntimeInfo::staticRuntimeInfoFilePath()
{
	return Dirs::exeDir() + "/" + staticInfoFileName;
}

std::string DynamicRuntimeInfo::dynamicRuntimeInfoFilePath()
{	
	switch (getRuntimeEnvironment()) {
	case RuntimeEnvironment::ZIP:
		return fq(AppDirs::programDir().absoluteFilePath(tq(dynamicInfoFileName)));
	case RuntimeEnvironment::MSIX:
	case RuntimeEnvironment::MSI:
		return fq(AppDirs::appData(false) + "/" + tq(dynamicInfoFileName));
	default:
		return "";
	}
}

bool DynamicRuntimeInfo::writeDynamicRuntimeInfoFile()
{
	std::string path = dynamicRuntimeInfoFilePath();
    std::ofstream out(path, std::ofstream::out);
    if(!out)
    {
		Log::log() << "Failed to open specified path for writing dynamic runtime info file" << std::endl;
        return false;
    }

    Json::Value root;
	root["jaspVersion"] = AppInfo::version.asString(4);
    root["initialized"] = true;
	root["commit"] = AppInfo::gitCommit;
	root["buildDate"] = AppInfo::builddate;
	root["RVersion"] = AppInfo::getRVersion();
	uint64_t timestamp = std::chrono::system_clock::now().time_since_epoch() / std::chrono::milliseconds(1);
	root["initTimestamp"] = timestamp;

	out << root;
	return true;
}

RuntimeEnvironment DynamicRuntimeInfo::getRuntimeEnvironment()
{
#ifdef FLATPAK_USED
	return RuntimeEnvironment::FLATPAK;
#elif __APPLE__
	return RuntimeEnvironment::MAC;
#elif linux
	return RuntimeEnvironment::LINUX_LOCAL;
#elif _WIN32
	std::string path = staticRuntimeInfoFilePath();
	Log::log() << "Attempting to read static runtime information from: " + path << std::endl;
	std::ifstream in(path, std::ifstream::in);
	if(!in)
	{
		Log::log() << "Failed to open specified static runtime file" << std::endl;
		return RuntimeEnvironment::UNKNOWN;
	}

	Json::Value root;
	in >> root;

	std::string runtimeEnvironmentString = root.get("runtimeEnvironment", "").asString();
	return RuntimeEnvironmentFromString(runtimeEnvironmentString, RuntimeEnvironment::UNKNOWN);
#else
	_environment = RuntimeEnvironment::UNKNOWN;
#endif

}

std::string DynamicRuntimeInfo::getRuntimeEnvironmentAsString()
{
	return RuntimeEnvironmentToString(getRuntimeEnvironment());
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

std::string DynamicRuntimeInfo::bundledModulesInitializedJaspVersion()
{
	return _initializedForJaspVersion;
}
