#include "modulelibrary.h"

#include <QString>

#include "appinfo.h"
#include "gui/preferencesmodel.h"
#include "installedmodules.h"
#include "dynamicmodules.h"
#include "dynamicmodule.h"
#include "utilities/dynamicruntimeinfo.h"

ModuleLibrary * ModuleLibrary::_singleton = nullptr;

ModuleLibrary::ModuleLibrary(QObject *parent)
    : QObject(parent)
{
    _singleton = this;

    if (auto *dynMods = Modules::DynamicModules::dynMods())
    {
        connect(dynMods, &Modules::DynamicModules::dynamicModuleAdded,      this, &ModuleLibrary::onDynamicModuleAdded);
        connect(dynMods, &Modules::DynamicModules::dynamicModuleChanged,    this, &ModuleLibrary::onDynamicModuleChanged);
        connect(dynMods, &Modules::DynamicModules::dynamicModuleReplaced,   this, &ModuleLibrary::onDynamicModuleReplaced);
        // TODO there is a timing issue here, the dynamicModuleUninstalled signal is emitted before the module is fully uninstalled,
        // causing UI to update too early and show the module as still installed.
        connect(dynMods, &Modules::DynamicModules::dynamicModuleUninstalled,this, &ModuleLibrary::onDynamicModuleUninstalled);
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
    
    auto platform = DynamicRuntimeInfo::getRuntimeEnvironment();
	auto arch = DynamicRuntimeInfo::getMicroArch();
    std::string platformArch;
	if(platform == RuntimeEnvironment::MAC)
		platformArch = arch == MicroArch::AARCH64 ? "MacOS_arm64" : "MacOS_x86_64";
	else if(platform == RuntimeEnvironment::FLATPAK)
		platformArch = arch == MicroArch::AARCH64 ? "Flatpak_aarch64" : "Flatpak_x86_64";
	else if(platform == RuntimeEnvironment::LINUX_LOCAL)
		platformArch = arch == MicroArch::AARCH64 ? "Linux_aarch64" : "Linux_x86_64";
	else
		platformArch = "Windows_x86-64";

    envInfo["arch"] = QString::fromStdString(platformArch);
    envInfo["developerMode"] = PreferencesModel::prefs()->developerMode();
    envInfo["theme"] = PreferencesModel::prefs()->currentThemeName().replace("Theme", "");
    envInfo["font"] = PreferencesModel::prefs()->interfaceFont();
    envInfo["language"] = PreferencesModel::prefs()->languageCode();

    envInfo["installedModules"] = installedModulesInfo();
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
        installedModules[QString::fromStdString(key)] = QString::fromStdString(val);
    return installedModules;
}

void ModuleLibrary::emitEnvironmentInfoChanged()
{
    emit environmentInfoChanged(getEnvironmentInfo());
}

void ModuleLibrary::onDynamicModuleAdded(Modules::DynamicModule *module)
{
    Q_UNUSED(module);
    emitEnvironmentInfoChanged();
}

void ModuleLibrary::onDynamicModuleChanged(Modules::DynamicModule *module)
{
    Q_UNUSED(module);
    emitEnvironmentInfoChanged();
}

void ModuleLibrary::onDynamicModuleReplaced(Modules::DynamicModule *oldModule, Modules::DynamicModule *module)
{
    Q_UNUSED(oldModule);
    Q_UNUSED(module);
    emitEnvironmentInfoChanged();
}

void ModuleLibrary::onDynamicModuleUninstalled(const QString &moduleName)
{
    Q_UNUSED(moduleName);
    emitEnvironmentInfoChanged();
}