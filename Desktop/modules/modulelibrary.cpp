#include "modulelibrary.h"

#include <QString>
#include <QDir>

#include "appinfo.h"
#include "gui/preferencesmodel.h"
#include "installedmodules.h"
#include "dynamicmodules.h"
#include "dynamicmodule.h"
#include "engine/enginesync.h"
#include "utilities/appdirs.h"
#include "utilities/dynamicruntimeinfo.h"

ModuleLibrary * ModuleLibrary::_singleton = nullptr;

ModuleLibrary::ModuleLibrary(QObject *parent)
    : QObject(parent)
{
    _singleton = this;

    if (auto *dynMods = Modules::DynamicModules::dynMods())
    {
        connect(dynMods, &Modules::DynamicModules::dynamicModuleAdded,      this, [this](Modules::DynamicModule *) { 
            emitEnvironmentInfoChanged(); 
            finishInstalling();
        });
        connect(dynMods, &Modules::DynamicModules::dynamicModuleChanged,    this, [this](Modules::DynamicModule *) { emitEnvironmentInfoChanged(); });
        connect(dynMods, &Modules::DynamicModules::dynamicModuleReplaced,   this, [this](Modules::DynamicModule *, Modules::DynamicModule *) { emitEnvironmentInfoChanged(); });       
    }
    if (auto *engineSync = EngineSync::singleton())
    {
        connect(engineSync, &EngineSync::moduleInstallationSucceeded, this, [this]() {
            emitEnvironmentInfoChanged(); 
            finishInstalling(); 
        });
        connect(engineSync, &EngineSync::moduleInstallationFailed, this, [this](const QString &, const QString &) {
            emitEnvironmentInfoChanged(); 
            finishInstalling(); 
        });
        connect(engineSync, &EngineSync::moduleUninstallationSucceeded, this, [this]() {
            emitEnvironmentInfoChanged(); 
            finishInstalling(); 
        });
        connect(engineSync, &EngineSync::moduleUninstallationFailed, this, [this](const QString &, const QString &) {
            emitEnvironmentInfoChanged(); 
            finishInstalling(); 
        });
    }

    if (auto *prefs = PreferencesModel::prefs())
    {
        connect(prefs, &PreferencesModel::developerModeChanged, this, [this](bool) { emitEnvironmentInfoChanged(); });
        connect(prefs, &PreferencesModel::languageCodeChanged,  this, [this]() { emitEnvironmentInfoChanged(); });
        connect(prefs, &PreferencesModel::interfaceFontChanged, this, [this]() { emitEnvironmentInfoChanged(); });
        connect(prefs, &PreferencesModel::currentThemeNameChanged,  this, [this](const QString &) { emitEnvironmentInfoChanged(); });
    }
}

QVariantMap ModuleLibrary::getEnvironmentInfo() const
{
    QVariantMap envInfo;
    envInfo["version"] = QString(AppInfo::version.asString(3).c_str());
    
    auto platform	= DynamicRuntimeInfo::getRuntimeEnvironment();
	auto arch		= DynamicRuntimeInfo::getMicroArch();
	
    std::string platformArch;
	if(platform == RuntimeEnvironment::MAC)					platformArch = arch == MicroArch::AARCH64 ? "MacOS_arm64" : "MacOS_x86_64";
	else if(platform == RuntimeEnvironment::FLATPAK)		platformArch = arch == MicroArch::AARCH64 ? "Flatpak_aarch64" : "Flatpak_x86_64";
	else if(platform == RuntimeEnvironment::LINUX_LOCAL)	platformArch = arch == MicroArch::AARCH64 ? "Flatpak_aarch64" : "Flatpak_x86_64";      // When developing within devcontainer then jaspModule files with Flatpak_x86_64 also work? No...
	else													platformArch = "Windows_x86-64";
	
    envInfo["arch"]					= tq(platformArch);
    envInfo["developerMode"]		= PreferencesModel::prefs()->developerMode();						// Preferences needed in webapp
    envInfo["theme"]				= PreferencesModel::prefs()->currentThemeName().replace("Theme", "");
    envInfo["font"]					= PreferencesModel::prefs()->interfaceFont();
    envInfo["language"]				= PreferencesModel::prefs()->languageCode().replace("_", "-");		// do replace to enforce BCP 47 language tag format
    envInfo["installedModules"]		= installedModulesInfo();
    envInfo["uninstallableModules"] = getUninstallableModules();
    return envInfo;
}

void ModuleLibrary::uninstallJASPModule(const QString &moduleName)
{
    if (auto *dynMods = Modules::DynamicModules::dynMods())
        dynMods->uninstallModule(moduleName.toStdString());
}

QVariantMap ModuleLibrary::installedModulesInfo() const
{
    QVariantMap installedModules;
    for (auto const& [key, val] : InstalledModules::getInstalledModuleVersions())
        installedModules[tq(key)] = tq(val);
    return installedModules;
}

QStringList ModuleLibrary::getUninstallableModules() const
{
    // Only modules installed in user modules dir are uninstallable
    auto dir = QDir(AppDirs::userModulesLibDir());
    return dir.entryList(QDir::Dirs | QDir::NoDotAndDotDot);
}

void ModuleLibrary::emitEnvironmentInfoChanged()
{
    emit environmentInfoChanged(getEnvironmentInfo());
}

void ModuleLibrary::startInstalling()
{
    _isInstalling = true;
    emit isInstallingChanged();
}

void ModuleLibrary::finishInstalling()
{
    _isInstalling = false;
    emit isInstallingChanged();
}
