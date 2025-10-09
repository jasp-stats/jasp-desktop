#include "modulelibrary.h"

#include <QString>

#include "appinfo.h"
#include "gui/preferencesmodel.h"
#include "installedmodules.h"

ModuleLibrary * ModuleLibrary::_singleton = nullptr;

ModuleLibrary::ModuleLibrary(QObject *parent)
    : QObject(parent)
{
    _singleton = this;
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

    QVariantMap installedModules;
    for (auto const& [key, val] : InstalledModules::getInstalledModuleVersions()) {
        installedModules[QString::fromStdString(key)] = QString::fromStdString(val);
    }
    envInfo["installedModules"] = installedModules;
    return envInfo;
}