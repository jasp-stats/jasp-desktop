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

class DynamicModules : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QString	currentInstallMsg	READ currentInstallMsg									NOTIFY currentInstallMsgChanged)
	Q_PROPERTY(QString	currentInstallName	READ currentInstallName									NOTIFY currentInstallNameChanged)
	Q_PROPERTY(bool		currentInstallDone	READ currentInstallDone	WRITE	setCurrentInstallDone	NOTIFY currentInstallDoneChanged)

public:
	explicit DynamicModules(QObject *parent) ;

	~DynamicModules()
	{
		for(auto dynamic : _modules)
			delete dynamic.second;
		_modules.clear();
	}

	std::string moduleDirectory(std::string moduleName)		{ return AppDirs::modulesDir().toStdString() + moduleName + '/'; }

	std::string installModule(std::string moduleZipFilename);
	void		uninstallModule(std::string modulePath);

	std::string	loadModule(std::string moduleName)			{ return loadModuleFromDir(moduleDirectory(moduleName)); }
	std::string	loadModuleFromDir(std::string moduleDir);
	void		unloadModule(std::string moduleName);
	void		loadInstalledModules();

	bool		moduleIsLoaded(std::string moduleName)		{ return _modules.count(moduleName) > 0;													}
	bool		moduleIsInstalled(std::string moduleName)	{ return boost::filesystem::exists(moduleDirectory(moduleName)); }

	bool		aModuleNeedsToBeLoadedInR()					{ return _modulesToBeLoaded.size() > 0; }
	bool		aModuleNeedsPackagesInstalled()				{ return _modulesInstallPackagesNeeded.size() > 0; }
	Json::Value	requestJsonForPackageLoadingRequest()		{ return requestModuleForSomethingAndRemoveIt(_modulesToBeLoaded)->requestJsonForPackageLoadingRequest(); }
	Json::Value	requestJsonForPackageInstallationRequest()	{ return requestModuleForSomethingAndRemoveIt(_modulesInstallPackagesNeeded)->requestJsonForPackageInstallationRequest(); }

	Modules::DynamicModule*	dynamicModule(std::string moduleName) { return _modules.count(moduleName) == 0 ? NULL : _modules[moduleName]; }

	Modules::AnalysisEntry* retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile);

	Q_INVOKABLE bool	isFileAnArchive(QString filepath);
	Q_INVOKABLE QString getDescriptionFromArchive(QString filepath);
	Q_INVOKABLE void	installJASPModule(QString filepath);

	QString currentInstallMsg()									{ return _currentInstallMsg; }
	void setCurrentInstallMsg(QString currentInstallMsg)		{ if(_currentInstallMsg != currentInstallMsg) { _currentInstallMsg = currentInstallMsg; emit currentInstallMsgChanged(); } }
	void setCurrentInstallMsg(std::string currentInstallMsg)	{ setCurrentInstallMsg(QString::fromStdString(currentInstallMsg)); }

	QString currentInstallName()								{ return _currentInstallName; }
	void setCurrentInstallName(QString currentInstallName)		{ if(_currentInstallName != currentInstallName) { _currentInstallName = currentInstallName; emit currentInstallNameChanged(); } }


	bool currentInstallDone()									{ return _currentInstallDone; }
	void setCurrentInstallDone(bool currentInstallDone);

	int numberOfModules()										{ return _modules.size(); }

	std::vector<std::string> moduleNames();

signals:
	void showModuleInstallerWindow(QString url);
	void currentInstallMsgChanged();
	void currentInstallNameChanged();
	void currentInstallDoneChanged();

private:
	Modules::DynamicModule* requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet);


public slots:
	void installationPackagesSucceeded(	std::string moduleName);
	void installationPackagesFailed(	std::string moduleName, std::string errorMessage);
	void loadingSucceeded(				std::string moduleName);
	void loadingFailed(					std::string moduleName, std::string errorMessage);
	void openModuleInstallerWindow();

private:
	std::map<std::string, Modules::DynamicModule*>	_modules;
	std::set<std::string>							_modulesInstallPackagesNeeded,
													_modulesToBeLoaded;
	boost::filesystem::path							_modulesInstallDirectory;
	QString											_currentInstallMsg = "",
													_currentInstallName = "";
	bool											_currentInstallDone = false;
};

#endif // DYNAMICMODULES_H
