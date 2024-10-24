//
// Copyright (C) 2013-2018 University of Amsterdam
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


#ifndef DYNAMICMODULES_H
#define DYNAMICMODULES_H

#include <map>
#include <set>
#include <filesystem>
#include <QObject>
#include "version.h"
#include "dynamicmodule.h"
#include <QFileSystemWatcher>
#include "upgrader/upgradeDefinitions.h"

namespace Modules
{

/// 
/// This class handles all dynamic modules and (un)loading them, as well as facilitating installation etc
/// 
class DynamicModules : public QObject
{
	Q_OBJECT

	Q_PROPERTY(bool			developersModuleInstallButtonEnabled	READ developersModuleInstallButtonEnabled	WRITE setDevelopersModuleInstallButtonEnabled	NOTIFY developersModuleInstallButtonEnabledChanged	)
	Q_PROPERTY(bool			dataLoaded								READ dataLoaded								WRITE setDataLoaded								NOTIFY dataLoadedChanged							)
	Q_PROPERTY(QStringList	loadedModules							READ loadedModules																			NOTIFY loadedModulesChanged							)
	Q_PROPERTY(QStringList	loadedModulesTitles						READ loadedModulesTitles																	NOTIFY loadedModulesChanged							)

public:
	explicit				DynamicModules(QObject *parent) ;
							~DynamicModules() override;
	static DynamicModules * dynMods()	{ return _singleton; }

	void					initializeInstalledModules();
	void					registerQMLTypes();

	bool					unpackAndInstallModule(		const	std::string & moduleZipFilename);
	void					uninstallModule(			const	std::string & moduleName);
	std::string				loadModule(					const	std::string & moduleName);
	void					unloadModule(				const	std::string & moduleName);
	bool					initializeModuleFromDir(			std::string   moduleDir,	bool bundled = false, bool isCommon = false);
	bool					initializeModule(					Modules::DynamicModule * module);
	void					replaceModule(						Modules::DynamicModule * module);

	static bool				bundledModuleInFilesystem(	const	std::string & moduleName);
	static std::string		bundledModuleLibraryPath(	const	std::string & moduleName);
	std::string				moduleDirectory(			const	std::string & moduleName)	const;
	std::wstring			moduleDirectoryW(			const	std::string & moduleName)	const;
	QString					moduleDirectoryQ(			const	QString     & moduleName)	const;

	bool					moduleIsInstalledByUser(	const	std::string & moduleName)	const { return std::filesystem::exists(moduleDirectoryW(moduleName));	}

	bool					moduleHasUpgradesToApply(	const	 std::string & module,		const std::string & function, const Version & version);
	void					applyUpgrade(				const	 std::string & module,		const std::string & function, const Version	& version, Json::Value & analysesJson, Modules::UpgradeMsgs & msgs, Modules::StepsTaken & stepsTaken);

	bool					aModuleNeedsPackagesInstalled()			const;
	size_t					numModulesNeedingPackagesInstalled()	const;
	stringset				modulesNeedingPackagesInstalled()		const;

	Json::Value				getJsonForPackageInstallationRequest(const std::string & module = "???");

	Modules::DynamicModule*	dynamicModuleLowerCased(	  QString		moduleName)	const;
	Modules::DynamicModule*	dynamicModule(			const std::string & moduleName)	const { return _modules.count(moduleName) == 0 ? nullptr : _modules.at(moduleName); }
	Modules::DynamicModule*	operator[](				const std::string & moduleName)	const { return dynamicModule(moduleName); }

	Modules::AnalysisEntry* retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile);

	Q_INVOKABLE bool		isFileAnArchive(				const QString & filepath);

	Q_INVOKABLE void		installJASPModule(				const QString & filepath);
	Q_INVOKABLE	void		uninstallJASPModule(			const QString & moduleName);
	Q_INVOKABLE void		installJASPDeveloperModule();

	Q_INVOKABLE QString		getDescriptionFormattedFromArchive(QString archiveFilePath);

	int numberOfModules()												{ return _modules.size(); }
	const std::vector<std::string> & moduleNames() const				{ return _moduleNames; }

	Q_INVOKABLE Modules::DynamicModule*	dynamicModule(QString moduleName) const { return dynamicModule(moduleName.toStdString()); }

	static std::string  developmentModuleName()			{ return Modules::DynamicModule::developmentModuleName(); }
	static std::string  defaultDevelopmentModuleName()	{ return Modules::DynamicModule::defaultDevelopmentModuleName(); }
	static QString		developmentModuleFolder()		{ return Modules::DynamicModule::developmentModuleFolder().absoluteFilePath(); }

	void startWatchingDevelopersModule();

	bool developersModuleInstallButtonEnabled() const { return _devModInstallButtonOn; }
	bool dataLoaded()							const { return _dataLoaded;	}

	void insertCommonModuleNames(std::set<std::string> commonModules) { for(const std::string & common : commonModules) _commonModuleNames.insert(common); };

	QStringList importPaths() const;

	const QStringList loadedModules() const;
	const QStringList loadedModulesTitles() const;

public slots:
	void installationPackagesSucceeded(	const QString		& moduleName);
	void installationPackagesFailed(	const QString		& moduleName, const QString & errorMessage);
	void registerForInstalling(			const std::string	& moduleName);
	void registerForInstallingModPkg(	const std::string	& moduleName);
	void setDevelopersModuleInstallButtonEnabled(bool developersModuleInstallButtonEnabled);
	void setDataLoaded(bool dataLoaded);
	void uninstallJASPDeveloperModule();
	void refreshDeveloperModule(bool R = true, bool Qml = true);

	QStringList requiredModulesLibPaths(QString moduleName);


signals:
	void dynamicModuleUninstalled(const QString & moduleName);
	void dynamicModuleAdded(			Modules::DynamicModule * dynamicModule);
	void dynamicModuleUnloadBegin(		Modules::DynamicModule * dynamicModule);
	void dynamicModuleChanged(			Modules::DynamicModule * dynamicModule);
	void dynamicModuleQmlChanged(		Modules::DynamicModule * dynamicModule);
	void descriptionReloaded(			Modules::DynamicModule * dynamicModule);
	void loadModuleTranslationFile(		Modules::DynamicModule * dynamicModule);
	void dynamicModuleReplaced(			Modules::DynamicModule * oldMod, Modules::DynamicModule *  newMod);
	void reloadQmlImportPaths();
	bool isModuleInstallRequestActive(const QString & moduleName) const;

	void reloadHelpPage();

	void developersModuleInstallButtonEnabledChanged(bool developersModuleInstallButtonEnabled);
	void moduleEnabledChanged(QString moduleName, bool enabled);
	void dataLoadedChanged(bool dataLoaded);
	void loadedModulesChanged();

private:
	void						removeUninstalledModuleFolder(const std::string & moduleName);
	Modules::DynamicModule	*	requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet);
	void						devModCopyDescription(QString filename);
	void						devModWatchFolder(QString folder, QFileSystemWatcher * & watcher);
	void						regenerateDeveloperModuleRPackage();
	void						registerForInstallingSubFunc(const std::string & moduleName, bool onlyModPkg);

private:
	static DynamicModules								*	_singleton;
	std::set<std::string>									_commonModuleNames;
	std::vector<std::string>								_moduleNames;
	std::map<std::string, Modules::DynamicModule*>			_modules;
	std::map<std::string, bool>								_modulesInstallPackagesNeeded; //bool true ==> only modPkg
	std::filesystem::path									_modulesInstallDirectory;
	QString													_currentInstallMsg			= "",
															_currentInstallName			= "";
	bool													_currentInstallDone			= false,
															_devModInstallButtonOn		= true,
															_dataLoaded					= false;
	QDir													_devModSourceDirectory;
	QFileSystemWatcher									*	_devModDescriptionWatcher	= nullptr,
														*	_devModUpgradesWatcher		= nullptr,
														*	_devModRWatcher				= nullptr,
														*	_devModHelpWatcher			= nullptr;
	Modules::DynamicModule								*	_devModule					= nullptr;
};

}

#endif // DYNAMICMODULES_H
