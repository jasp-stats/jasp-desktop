#include "modulelibrary.h"

#include <QString>

#include "appinfo.h"
#include "gui/preferencesmodel.h"
#include "installedmodules.h"
#include "dynamicmodules.h"
#include "dynamicmodule.h"

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
}

QVariantMap ModuleLibrary::getEnvironmentInfo() const
{
    QVariantMap envInfo;
    envInfo["version"] = QString(AppInfo::version.asString(3).c_str());
    
    // TODO get arch instead of hardcoding to windows
    // See moduleStoreUrl in Desktop/modules/dynamicmodules.cpp
    QString platformString = "Windows_x86-64";
    envInfo["arch"] = platformString;
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

void ModuleLibrary::onDynamicModuleAdded(Modules::DynamicModule *module)
{
    Q_UNUSED(module);
    emit installedModulesChanged(installedModulesInfo());
}

void ModuleLibrary::onDynamicModuleChanged(Modules::DynamicModule *module)
{
    Q_UNUSED(module);
    emit installedModulesChanged(installedModulesInfo());
}

void ModuleLibrary::onDynamicModuleReplaced(Modules::DynamicModule *oldModule, Modules::DynamicModule *module)
{
    Q_UNUSED(oldModule);
    Q_UNUSED(module);
    emit installedModulesChanged(installedModulesInfo());
}

void ModuleLibrary::onDynamicModuleUninstalled(const QString &moduleName)
{
    Q_UNUSED(moduleName);
    emit installedModulesChanged(installedModulesInfo());
}