#ifndef DYNRUNTIMEINFO_H
#define DYNRUNTIMEINFO_H

/*! 
 *  Simple class that parses Runtime information from staticRuntimeInfo.json located in the install folder
 *  and dynamicRuntimeInfo.json located in a user directory defined by Appdirs. 
 *  staticRuntimeInfo.json containts information on install type. 
 * 	dynamicRuntimeInfo.json is used to log various information regarding the initialization of the environment. e.g. at first run
 * 	This class is used to query runtime information and to determine if there has been proper initialization for this JASP version.
*/

#include <cstdint>
#include <string>
#include <map>

class DynamicRuntimeInfo 
{
public:
	enum RuntimeEnvironment{ ZIP, MSIX, MSI, R, FLATPAK, MAC, LINUX_LOCAL, UNKNOWN };
	const std::map<std::string, RuntimeEnvironment> StringToRuntimeEnvironmentMap = {
        {"ZIP", RuntimeEnvironment::ZIP}, {"MSIX", RuntimeEnvironment::MSIX},
        {"MSI", RuntimeEnvironment::MSI}, {"R", RuntimeEnvironment::R},
        {"FLATPAK", RuntimeEnvironment::FLATPAK}, {"MAC", RuntimeEnvironment::MAC},
		{"LINUX_LOCAL", RuntimeEnvironment::LINUX_LOCAL}, {"UNKNOWN", RuntimeEnvironment::UNKNOWN},
	};
	const std::map<RuntimeEnvironment, std::string> RuntimeEnvironmentToStringMap = {
		{RuntimeEnvironment::ZIP, "ZIP"}, {RuntimeEnvironment::MSIX, "MSIX"},
		{RuntimeEnvironment::MSI, "MSI"}, {RuntimeEnvironment::R, "R"},
		{RuntimeEnvironment::FLATPAK, "FLATPAK"}, {RuntimeEnvironment::MAC, "MAC"},
		{RuntimeEnvironment::LINUX_LOCAL, "LINUX_LOCAL"}, {RuntimeEnvironment::UNKNOWN, "UNKNOWN"},
		};

    enum MicroArch{ AARCH64, X86_64, UNSUPPORTED };

	bool bundledModulesInitialized();

	RuntimeEnvironment getRuntimeEnvironment();
	std::string getRuntimeEnvironmentAsString();
	RuntimeEnvironment getMicroArch();
	uint64_t bundledModulesInitializedOnTimestamp();
    std::string bundledModulesInitializedByCommit();
	std::string bundledModulesInitializedByBuildDate();
    std::string bundledModulesInitializedRVersion();
	std::string bundledModulesInitializedJaspVersion();

	bool writeDynamicRuntimeInfoFile();

    //singleton stuff
    static DynamicRuntimeInfo* getInstance();
	DynamicRuntimeInfo(DynamicRuntimeInfo& other) = delete;
	void operator=(const DynamicRuntimeInfo&) = delete;

protected:
    DynamicRuntimeInfo();

    bool parseStaticRuntimeInfoFile(const std::string& path);
    bool parseDynamicRuntimeInfoFile(const std::string& path);

private:
	std::string staticRuntimeInfoFilePath();
	std::string dynamicRuntimeInfoFilePath();

    RuntimeEnvironment _environment;
    MicroArch _arch;

	bool _bundledModulesInitializedSet = true;
    std::string _initializedByCommit = "build";
    std::string _initializedForRVersion = "build";
	std::string _initializedByBuildDate = "build";
	std::string _initializedForJaspVersion = "build";
    uint64_t _initializedOn = 0;

    static DynamicRuntimeInfo* _instance;

	static const std::string staticInfoFileName;
	static const std::string dynamicInfoFileName;
};

#endif // DYNRUNTIMEINFO_H
