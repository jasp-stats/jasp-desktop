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

public:
	explicit DynamicModules(QObject *parent) ;

	~DynamicModules() override;

	void		initializeInstalledModules();

	bool		unpackAndInitializeModule(				const	std::string & moduleZipFilename);
	void		uninstallModule(			const	std::string & moduleName);
	std::string	loadModule(					const	std::string & moduleName);
	bool		initializeModuleFromDir(			std::string   moduleDir);
	void		unloadModule(				const	std::string & moduleName);

	std::string moduleDirectory(			const	std::string & moduleName)	const { return AppDirs::modulesDir().toStdString() + moduleName + '/';	}
	bool		moduleIsInitialized(		const	std::string & moduleName)	const { return _modules.count(moduleName) > 0;							}
	bool		moduleIsInstalled(			const	std::string & moduleName)	const { return boost::filesystem::exists(moduleDirectory(moduleName));	}

	bool		aModuleNeedsToBeLoadedInR()					{ return !_modulesToBeLoaded.empty();				}
	bool		aModuleNeedsToBeUnloadedFromR()				{ return !_modulesToBeUnloaded.empty();				}
	bool		aModuleNeedsPackagesInstalled()				{ return !_modulesInstallPackagesNeeded.empty();	}

	Json::Value	getJsonForPackageLoadingRequest()		{ return requestModuleForSomethingAndRemoveIt(_modulesToBeLoaded)->requestJsonForPackageLoadingRequest();					}
	Json::Value getJsonForPackageUnloadingRequest();
	Json::Value	getJsonForPackageInstallationRequest()	{ return requestModuleForSomethingAndRemoveIt(_modulesInstallPackagesNeeded)->requestJsonForPackageInstallationRequest();	}
	Json::Value	getJsonForReloadingActiveModules();

	Modules::DynamicModule*	dynamicModule(	const std::string & moduleName)	const { return _modules.count(moduleName) == 0 ? nullptr : _modules.at(moduleName); }
	Modules::DynamicModule*	operator[](		const std::string & moduleName)		const { return dynamicModule(moduleName); }

	Modules::AnalysisEntry* retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile);
	Modules::AnalysisEntry*	retrieveCorrespondingAnalysisEntry(const std::string & codedReference);

	Q_INVOKABLE bool	isFileAnArchive(			const QString & filepath);
	Q_INVOKABLE QString getDescriptionFromArchive(	const QString & filepath);
	Q_INVOKABLE QString	installJASPModule(			const QString & filepath);
	Q_INVOKABLE	void	uninstallJASPModule(		const QString & moduleName);
	Q_INVOKABLE void	installJASPDeveloperModule();

	int numberOfModules()												{ return _modules.size(); }
	const std::vector<std::string> & moduleNames() const				{ return _moduleNames; }

	Q_INVOKABLE Modules::DynamicModule*	dynamicModule(QString moduleName) const { return dynamicModule(moduleName.toStdString()); }

	static std::string developmentModuleName() { return "DevelopmentModule"; }

public slots:
	void installationPackagesSucceeded(	const std::string & moduleName);
	void installationPackagesFailed(	const std::string & moduleName, const std::string & errorMessage);
	void loadingSucceeded(				const std::string & moduleName);
	void loadingFailed(					const std::string & moduleName, const std::string & errorMessage);
	void registerForInstalling(			const std::string & moduleName);
	void registerForLoading(			const std::string & moduleName);

signals:
	void dynamicModuleAdded(Modules::DynamicModule * dynamicModule);
	void dynamicModuleUninstalled(const std::string & moduleName);
	void dynamicModuleUnloadBegin(Modules::DynamicModule * dynamicModule);
	void dynamicModuleChanged(Modules::DynamicModule * dynamicModule);
	void descriptionReloaded(Modules::DynamicModule * dynMod);

	void stopEngines();
	void restartEngines();

	void reloadHelpPage();

private slots:
	void enginesStopped();

private:
	void						removeUninstalledModuleFolder(const std::string & moduleName, bool enginesStopped = false);
	Modules::DynamicModule	*	requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet);
	void						devModCopyDescription();
	void						devModCopyFolder(QString folder, QFileSystemWatcher * & watcher);
	void						regenerateDeveloperModuleRPackage();

private:
	std::vector<std::string>								_moduleNames;
	std::map<std::string, Modules::DynamicModule*>			_modules;
	std::set<std::string>									_modulesInstallPackagesNeeded,
															_modulesToBeLoaded;
	std::map<std::string, Json::Value>						_modulesToBeUnloaded;
	boost::filesystem::path									_modulesInstallDirectory;
	QString													_currentInstallMsg = "",
															_currentInstallName = "";
	bool													_currentInstallDone = false;
	const std::map<std::string, std::set<std::string>>		_acceptedFilesInFolders = {{"", {"json"}}, {"r", {"r"}}, {"qml", {"qml"}}, {"icons", {"svg", "png", "ico", "jpg", "gif"}}, {"help", {"md", "html"}}};
	QDir													_devModSourceDirectory;
	QFileSystemWatcher									*	_devModDescriptionWatcher	= nullptr,
														*	_devModRWatcher				= nullptr,
														*	_devModQmlWatcher			= nullptr,
														*	_devModIconsWatcher			= nullptr,
														*	_devModHelpWatcher			= nullptr;
};

#endif // DYNAMICMODULES_H
