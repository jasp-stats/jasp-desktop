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
#include <QObject>
#include "dynamicmodule.h"
#include <boost/filesystem.hpp>
#include <QFileSystemWatcher>

class DynamicModules : public QObject
{
	Q_OBJECT

	Q_PROPERTY(bool developersModuleInstallButtonEnabled	READ developersModuleInstallButtonEnabled	WRITE setDevelopersModuleInstallButtonEnabled	NOTIFY developersModuleInstallButtonEnabledChanged	)
	Q_PROPERTY(bool dataLoaded								READ dataLoaded								WRITE setDataLoaded								NOTIFY dataLoadedChanged							)

public:
	explicit DynamicModules(QObject *parent) ;

	static DynamicModules * dynMods() { return _singleton; }

	~DynamicModules() override;

	void		initializeInstalledModules();

	bool		unpackAndInstallModule(		const	std::string & moduleZipFilename);
	void		uninstallModule(			const	std::string & moduleName);
	std::string	loadModule(					const	std::string & moduleName);
	bool		initializeModuleFromDir(			std::string   moduleDir);
	void		unloadModule(				const	std::string & moduleName);
	bool		initializeModule(					Modules::DynamicModule * module);

	std::string moduleDirectory(			const	std::string & moduleName)	const;
	std::wstring moduleDirectoryW(			const	std::string & moduleName)	const;
	bool		moduleIsInstalled(			const	std::string & moduleName)	const { return boost::filesystem::exists(moduleDirectoryW(moduleName));	}

	bool		aModuleNeedsToBeLoadedInR()					{ return !_modulesToBeLoaded.empty();				}
	bool		aModuleNeedsToBeUnloadedFromR()				{ return !_modulesToBeUnloaded.empty();				}
	bool		aModuleNeedsPackagesInstalled()				{ return !_modulesInstallPackagesNeeded.empty();	}

	Json::Value	getJsonForPackageLoadingRequest()		{ return requestModuleForSomethingAndRemoveIt(_modulesToBeLoaded)->requestJsonForPackageLoadingRequest();					}
	Json::Value getJsonForPackageUnloadingRequest();
	Json::Value	getJsonForPackageInstallationRequest();
	Json::Value	getJsonForReloadingActiveModules();

	Modules::DynamicModule*	dynamicModule(	const std::string & moduleName)	const { return _modules.count(moduleName) == 0 ? nullptr : _modules.at(moduleName); }
	Modules::DynamicModule*	operator[](		const std::string & moduleName)		const { return dynamicModule(moduleName); }

	Modules::AnalysisEntry* retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile);

	Q_INVOKABLE bool	isFileAnArchive(				const QString & filepath);

	Q_INVOKABLE void	installJASPModule(				const QString & filepath);
	Q_INVOKABLE	void	uninstallJASPModule(			const QString & moduleName);
	Q_INVOKABLE void	installJASPDeveloperModule();

	Q_INVOKABLE QString getDescriptionJsonFromArchive(QString archiveFilePath);

	int numberOfModules()												{ return _modules.size(); }
	const std::vector<std::string> & moduleNames() const				{ return _moduleNames; }

	Q_INVOKABLE Modules::DynamicModule*	dynamicModule(QString moduleName) const { return dynamicModule(moduleName.toStdString()); }

	static std::string  developmentModuleName()			{ return Modules::DynamicModule::developmentModuleName(); }
	static std::string  defaultDevelopmentModuleName()	{ return Modules::DynamicModule::defaultDevelopmentModuleName(); }
	static QString		developmentModuleFolder()		{ return Modules::DynamicModule::developmentModuleFolder().absoluteFilePath(); }

	void startWatchingDevelopersModule();

	bool developersModuleInstallButtonEnabled() const { return _developersModuleInstallButtonEnabled; }
	bool dataLoaded()							const { return _dataLoaded;	}

public slots:
	void installationPackagesSucceeded(	const QString & moduleName);
	void installationPackagesFailed(	const QString & moduleName, const QString & errorMessage);
	void loadingSucceeded(				const QString & moduleName);
	void loadingFailed(					const QString & moduleName, const QString & errorMessage);
	void registerForInstalling(			const std::string & moduleName);
	void registerForInstallingModPkg(	const std::string & moduleName);

	void registerForLoading(			const std::string & moduleName);
	void setDevelopersModuleInstallButtonEnabled(bool developersModuleInstallButtonEnabled);
	void setDataLoaded(bool dataLoaded);

signals:
	void dynamicModuleAdded(Modules::DynamicModule * dynamicModule);
	void dynamicModuleUninstalled(const QString & moduleName);
	void dynamicModuleUnloadBegin(Modules::DynamicModule * dynamicModule);
	void dynamicModuleChanged(Modules::DynamicModule * dynamicModule);
	void descriptionReloaded(Modules::DynamicModule * dynMod);
	//void loadModuleTranslationFile(QString filename, QString location);
	void loadModuleTranslationFile(Modules::DynamicModule * dynamicModule);

	void stopEngines();
	void restartEngines();

	void reloadHelpPage();

	void developersModuleInstallButtonEnabledChanged(bool developersModuleInstallButtonEnabled);
	void moduleEnabledChanged(QString moduleName, bool enabled);
	void dataLoadedChanged(bool dataLoaded);

	QObject * loadQmlData(QString data, QUrl url);

	QQmlContext * requestRootContext();

private slots:
	void enginesStopped();

private:
	void						removeUninstalledModuleFolder(const std::string & moduleName, bool enginesStopped = false);
	Modules::DynamicModule	*	requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet);
	void						devModCopyDescription(QString filename);
	void						devModWatchFolder(QString folder, QFileSystemWatcher * & watcher);
	void						regenerateDeveloperModuleRPackage();
	void						registerForInstallingSubFunc(const std::string & moduleName, bool onlyModPkg);

private:
	static DynamicModules								*	_singleton;
	std::vector<std::string>								_moduleNames;
	std::map<std::string, Modules::DynamicModule*>			_modules;
	std::map<std::string, bool>								_modulesInstallPackagesNeeded; //bool true ==> only modPkg
	std::set<std::string>									_modulesToBeLoaded;
	std::map<std::string, Json::Value>						_modulesToBeUnloaded;
	boost::filesystem::path									_modulesInstallDirectory;
	QString													_currentInstallMsg = "",
															_currentInstallName = "";
	bool													_currentInstallDone = false;
	const std::map<std::string, std::set<std::string>>		_acceptedFilesInFolders = {{"", {"json"}}, {"r", {"r"}}, {"qml", {"qml"}}, {"icons", {"svg", "png", "ico", "jpg", "gif"}}, {"help", {"md", "html"}}};
	QDir													_devModSourceDirectory;
	QFileSystemWatcher									*	_devModDescriptionWatcher	= nullptr,
														*	_devModRWatcher				= nullptr,
														*	_devModHelpWatcher			= nullptr;
	Modules::DynamicModule								*	_devModule					= nullptr;
	bool													_developersModuleInstallButtonEnabled = true,
															_dataLoaded = false;
};

#endif // DYNAMICMODULES_H
