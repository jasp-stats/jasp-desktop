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


#include "dynamicmodules.h"
#include "utilities/extractarchive.h"

DynamicModules::DynamicModules(QObject *parent) : QObject(parent)
{
	_modulesInstallDirectory = AppDirs::modulesDir().toStdString();

	if(!boost::filesystem::exists(_modulesInstallDirectory))
		boost::filesystem::create_directories(_modulesInstallDirectory);

	loadInstalledModules();
}

void DynamicModules::loadInstalledModules()
{
	boost::system::error_code error;
	for (boost::filesystem::directory_iterator itr(_modulesInstallDirectory, error); !error && itr != boost::filesystem::directory_iterator(); itr++)
	{
		std::string path = itr->path().generic_string();
		std::string name = itr->path().filename().generic_string();
		if(name.size() > 0 && name[0] != '.')
			loadModuleFromDir(path);
	}
}

void DynamicModules::uninstallModule(std::string moduleName)
{
	std::string modulePath	= moduleDirectory(moduleName);

	if(moduleIsLoaded(moduleName))
		unloadModule(moduleName);

	if(boost::filesystem::exists(modulePath))
		boost::filesystem::remove_all(modulePath);
}

std::string DynamicModules::installModule(std::string moduleZipFilename)
{
	if(!QFile(QString::fromStdString(moduleZipFilename)).exists())
	{
		setCurrentInstallMsg("Cannot install module because " + moduleZipFilename + " does not exist.");
		setCurrentInstallDone(true);
	}

	auto beginPos				= moduleZipFilename.find_last_of('/') + 1;
	std::string moduleArchive	= moduleZipFilename.substr(beginPos == std::string::npos ? 0 : beginPos);
	std::string moduleName		= Modules::DynamicModule::moduleNameFromFolder(moduleArchive.substr(0, moduleArchive.find_last_of('.')));
	std::string modulePath		= AppDirs::modulesDir().toStdString() + moduleName;

	if(moduleIsInstalled(moduleName))
		uninstallModule(moduleName);

	if(!ExtractArchive::extractArchiveToFolderFlattened(moduleZipFilename, modulePath, {"R", "qml", "icons"}))
	{
		setCurrentInstallMsg("There was some error installing module from " + moduleZipFilename);
		setCurrentInstallDone(true);
	}

	return loadModule(moduleName);
}

void DynamicModules::unloadModule(std::string moduleName)
{
	_modulesInstallPackagesNeeded.erase(moduleName);
	_modulesToBeLoaded.erase(moduleName);

	if(_modules.count(moduleName) > 0)
		delete _modules[moduleName];
	_modules.erase(moduleName);
}

std::string DynamicModules::loadModuleFromDir(std::string moduleDir)
{
	try
	{
		if(moduleDir.size() == 0)
			throw std::runtime_error("Empty path was supplied to DynamicsModules::loadModule..");

		if(moduleDir[moduleDir.size() - 1] != '/')
			moduleDir += '/';

		Modules::DynamicModule	*newMod	= new Modules::DynamicModule(QString::fromStdString(moduleDir), this);
		std::string				modName = newMod->name();


		if(newMod->installNeeded() && _currentInstallName.toStdString() == modName && newMod->requiredPackages().size() > 0)
		{
			std::string newMsg("");
			for(Json::Value & packageEntry : newMod->requiredPackages())
				newMsg+= (newMsg.size() > 0 ? ", " : "") + packageEntry["package"].asString();

			newMsg += (newMsg.size() > 0 ? " and " : "") + newMod->generatedPackageName();

			setCurrentInstallMsg(QString::fromStdString("Installing required package(s) for module: " + newMsg));
		}

		if(moduleIsLoaded(modName))
			unloadModule(modName);

		_modules[modName] = newMod;

		if(newMod->installNeeded())			_modulesInstallPackagesNeeded.insert(modName);
		else if(newMod->loadingNeeded())	_modulesToBeLoaded.insert(modName);
		else								throw std::runtime_error("A module("+ newMod->name() +") that does not require installing or loading was \"loaded\" into JASP, this can't be right.");

		return modName;
	}
	catch(std::runtime_error e)
	{
		std::cerr << "An error occured trying to load a module from dir " << moduleDir << ", the error was: " << e.what();
		return "";
	}
}

Modules::DynamicModule* DynamicModules::requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet)
{
	if(theSet.size() == 0)
		return NULL;

	std::string installMe = *theSet.begin();
	theSet.erase(installMe);

	return _modules[installMe];
}

void DynamicModules::installationPackagesFailed(std::string moduleName, std::string errorMessage)
{
	if(moduleName == currentInstallName().toStdString())
	{
		setCurrentInstallMsg("Installing packages for module (" + moduleName + ") failed because of: " + errorMessage);
		setCurrentInstallDone(true);
	}

	_modules[moduleName]->setInstalled(false);

}

void DynamicModules::installationPackagesSucceeded(std::string moduleName)
{
	if(moduleName == currentInstallName().toStdString())
	{
		setCurrentInstallMsg( "Installing packages for module (" + moduleName + ") succeeded!");
		setCurrentInstallDone(true);
	}

	std::cout << "Installing packages for module (" << moduleName<< ") succeeded!" << std::endl;
	_modules[moduleName]->setInstalled(true);

	if(_modules[moduleName]->loadingNeeded())	_modulesToBeLoaded.insert(moduleName);
	else										throw std::runtime_error("Unexpected! Module "+moduleName+" was just installed but doesn't want to be loaded..");
}


void DynamicModules::loadingFailed(std::string moduleName, std::string errorMessage)
{
	std::cout << "Loading packages for module (" << moduleName << ") failed because of: " << errorMessage << std::endl;
	_modules[moduleName]->setLoaded(false);
}

void DynamicModules::loadingSucceeded(std::string moduleName)
{
	std::cout << "Loading packages for module (" << moduleName<< ") succeeded!" << std::endl;
	_modules[moduleName]->setLoaded(true);
}

Modules::AnalysisEntry* DynamicModules::retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile)
{
	std::string moduleName		= jsonFromJaspFile.get("moduleName", "Modulename wasn't actually filled!").asString();

	if(_modules.count(moduleName) > 0)
		return _modules[moduleName]->retrieveCorrespondingAnalysisEntry(jsonFromJaspFile);

	throw Modules::ModuleException(moduleName, "Couldn't find Module " + moduleName +", to use this JASP file you will need to install that first.\nTry the module's website: "  + jsonFromJaspFile.get("moduleWebsite", "jasp-stats.org").asString()	 +  " or, if that doesn't help, you could try to contact the module's maintainer: " + jsonFromJaspFile.get("moduleAuthor", "the JASP team").asString());
}

void DynamicModules::openModuleInstallerWindow()
{
	emit showModuleInstallerWindow("qrc:///modules/ModuleInstaller.qml");
}

bool DynamicModules::isFileAnArchive(QString filepath)
{
	return ExtractArchive::isFileAnArchive(filepath.toStdString());
}

QString DynamicModules::getDescriptionFromArchive(QString filepath)
{
	return QString::fromStdString(ExtractArchive::extractSingleTextFileFromArchive(filepath.toStdString(), "description.json"));
}

void DynamicModules::installJASPModule(QString filepath)
{
	std::string path	= filepath.toStdString();
	size_t slash		= path.find_last_of('/');
	std::string name	= Modules::DynamicModule::moduleNameFromFolder(path.substr(slash == std::string::npos ? 0 : slash + 1));
	name				= name.substr(0, name.find_last_of('.')); //remove .jaspMod

	setCurrentInstallMsg(QString(""));
	setCurrentInstallDone(false);
	setCurrentInstallName(QString::fromStdString(name));

	installModule(path);
}

void DynamicModules::setCurrentInstallDone(bool currentInstallDone)
{
	if(_currentInstallDone != currentInstallDone)
	{
		_currentInstallDone = currentInstallDone;
		emit currentInstallDoneChanged();

		if(currentInstallDone)
			setCurrentInstallName("");
	}
}

std::vector<std::string> DynamicModules::moduleNames()
{
	std::vector<std::string> names;

	for(auto dynamic : _modules) {
        names.push_back(dynamic.first);
	}

	return names;
}
