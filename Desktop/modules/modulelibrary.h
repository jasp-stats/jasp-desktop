//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef MODULELIBRARY_H
#define MODULELIBRARY_H

#include <QObject>
#include <QVariant>
#include <QQmlWebChannel>

namespace Modules
{
class DynamicModule;
}

class ModuleLibrary : public QObject
{
    Q_OBJECT

public:
    explicit ModuleLibrary(QObject *parent = 0);

    static ModuleLibrary * singleton() { return _singleton; }

    Q_INVOKABLE QVariantMap getEnvironmentInfo() const;
    Q_INVOKABLE void uninstallJASPModule(const QString &moduleName);

signals:
    void installedModulesChanged(const QVariantMap &installedModules);

private:
    QVariantMap installedModulesInfo() const;

private slots:
    void onDynamicModuleAdded(Modules::DynamicModule *module);
    void onDynamicModuleChanged(Modules::DynamicModule *module);
    void onDynamicModuleReplaced(Modules::DynamicModule *oldModule, Modules::DynamicModule *module);
    void onDynamicModuleUninstalled(const QString &moduleName);

private:
    static ModuleLibrary *_singleton;
};

#endif // MODULELIBRARY_H