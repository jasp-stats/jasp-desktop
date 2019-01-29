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
#include "utilities/settings.h"

DynamicModules::DynamicModules(QObject *parent) : QObject(parent)
{
	_modulesInstallDirectory = AppDirs::modulesDir().toStdString();

	if(!boost::filesystem::exists(_modulesInstallDirectory))
		boost::filesystem::create_directories(_modulesInstallDirectory);

	initializeInstalledModules();
}

void DynamicModules::initializeInstalledModules()
{
	boost::system::error_code error;
	for (boost::filesystem::directory_iterator itr(_modulesInstallDirectory, error); !error && itr != boost::filesystem::directory_iterator(); itr++)
	{
		std::string path = itr->path().generic_string();
		std::string name = itr->path().filename().generic_string();
		if(name.size() > 0 && name[0] != '.')
			initializeModuleFromDir(path);
	}
}

bool DynamicModules::initializeModuleFromDir(std::string moduleDir)
{
	try
	{
		if(moduleDir.size() == 0)
			throw std::runtime_error("Empty path was supplied to DynamicsModules::loadModule..");

		if(moduleDir[moduleDir.size() - 1] != '/')
			moduleDir += '/';

		Modules::DynamicModule	*newMod		= new Modules::DynamicModule(QString::fromStdString(moduleDir), this);
		std::string				moduleName	= newMod->name();

		if(moduleIsInitialized(moduleName))
			throw std::runtime_error("Module "+ moduleName +" already initialized!");

		_modules[moduleName] = newMod;
		_moduleNames.push_back(moduleName);

		emit dynamicModuleAdded(newMod);

		if(newMod->installNeeded())
		{
			checkForInstallMsg(newMod);
			_modulesInstallPackagesNeeded.insert(moduleName);
		}

		return true;
	}
	catch(std::runtime_error & e)
	{
		std::cerr << "An error occured trying to initialize a module from dir " << moduleDir << ", the error was: " << e.what();
		return false;
	}
}

void DynamicModules::checkForInstallMsg(Modules::DynamicModule	* currentModule)
{
	if(currentModule->installNeeded() && _currentInstallName.toStdString() == currentModule->name() && currentModule->requiredPackages().size() > 0)
	{
		std::string newMsg("");
		for(Json::Value & packageEntry : currentModule->requiredPackages())
			newMsg+= (newMsg.size() > 0 ? ", " : "") + packageEntry["package"].asString();

		newMsg += (newMsg.size() > 0 ? " and " : "") + currentModule->generatedPackageName();

		setCurrentInstallMsg(QString::fromStdString("Installing required package(s) for module: " + newMsg + (currentModule->requiredPackages().size() > 0  ? " (this might take a while)" : "")));
	}
}

std::string DynamicModules::loadModule(std::string moduleName)
{
	try
	{
		if(_modules.count(moduleName) == 0 && !initializeModuleFromDir(moduleDirectory(moduleName)))
			throw std::runtime_error("Couldn't load (and initialize) module " + moduleName);


		Modules::DynamicModule	*loadMe	= _modules[moduleName];

		if(loadMe->installNeeded())			{ checkForInstallMsg(loadMe); _modulesInstallPackagesNeeded.insert(moduleName);}
		else if(loadMe->loadingNeeded())	registerForLoading(moduleName);
		else								throw std::runtime_error("A module("+ loadMe->name() +") that does not require installing or loading was \"loaded\" into JASP, this can't be right.");

		return moduleName;
	}
	catch(std::runtime_error & e)
	{
		std::cerr << "An error occured trying to load module " << moduleName << ", the error was: " << e.what();
		return "";
	}
}

void DynamicModules::registerForLoading(std::string moduleName)
{
	_modulesToBeUnloaded.erase(moduleName);
	_modulesToBeLoaded.insert(moduleName);
}

bool DynamicModules::installModule(std::string moduleZipFilename)
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

	return initializeModuleFromDir(modulePath);
}

void DynamicModules::unloadModule(std::string moduleName)
{
	_modulesInstallPackagesNeeded.erase(moduleName);
	_modulesToBeLoaded.erase(moduleName);

	if(_modules.count(moduleName) > 0)
	{
		Modules::DynamicModule * dynMod		= _modules[moduleName];
		_modulesToBeUnloaded[moduleName]	= dynMod->requestJsonForPackageUnloadingRequest();

		dynMod->unloadModule();

		emit dynamicModuleUnloadBegin(dynMod);
	}
}

void DynamicModules::uninstallModule(std::string moduleName)
{
	std::string modulePath	= moduleDirectory(moduleName);

	if(moduleIsInitialized(moduleName))
		unloadModule(moduleName);

	if(_modules.count(moduleName) > 0)
	{
		for(int i=int(_moduleNames.size()) - 1; i>=0; i--)
			if(_moduleNames[size_t(i)] == moduleName)
				_moduleNames.erase(_moduleNames.begin() + i);

		delete _modules[moduleName];
		_modules.erase(moduleName);
	}

	try
	{
		if(boost::filesystem::exists(modulePath))
			boost::filesystem::remove_all(modulePath);

	} catch (boost::filesystem::filesystem_error & e) {
		std::cerr << "Something went wrong removing files for module " << moduleName << " at path '" << modulePath << "' and the error was: " << e.what() << std::endl;
	}

	emit dynamicModuleUninstalled(moduleName);
}

Modules::DynamicModule* DynamicModules::requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet)
{
	if(theSet.size() == 0)
		return NULL;

	std::string installMe = *theSet.begin();
	theSet.erase(installMe);

	return _modules[installMe];
}

Json::Value DynamicModules::requestJsonForPackageUnloadingRequest()
{
	std::string firstModule		= _modulesToBeUnloaded.begin()->first;
	Json::Value	unloadRequest	= _modulesToBeUnloaded[firstModule];

	_modulesToBeUnloaded.erase(firstModule);

	return unloadRequest;
}

void DynamicModules::installationPackagesFailed(std::string moduleName, std::string errorMessage)
{
	if(moduleName == currentInstallName().toStdString())
	{
		setCurrentInstallMsg("Installing packages for module (" + moduleName + ") failed because of: " + errorMessage);
		setCurrentInstallDone(true);
	}

	if(_modules.count(moduleName) > 0)
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

	if(_modules[moduleName]->loadingNeeded())	registerForLoading(moduleName);
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

Modules::AnalysisEntry*	DynamicModules::retrieveCorrespondingAnalysisEntry(const std::string & codedReference)
{
    auto parts = stringUtils::splitString(codedReference, '~');

	if(parts.size() != 3)
		throw Modules::ModuleException("No module", "This isnt a coded reference");

	std::string moduleName		= parts[0],
				ribbonTitle		= parts[1],
				analysisTitle	= parts[2];

	return dynamicModule(moduleName)->retrieveCorrespondingAnalysisEntry(ribbonTitle, analysisTitle);
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

void DynamicModules::uninstallJASPModule(QString moduleName)
{
	uninstallModule(moduleName.toStdString());
}

void DynamicModules::installJASPDeveloperModule()
{
	std::string path	= Settings::value(Settings::DEVELEPER_FOLDER).toString().toStdString();
	size_t slash		= path.find_last_of('/');
	std::string name	= Modules::DynamicModule::moduleNameFromFolder(path.substr(slash == std::string::npos ? 0 : slash + 1));

	setCurrentInstallMsg(QString(""));
	setCurrentInstallDone(false);
	setCurrentInstallName(QString::fromStdString(name));

	if(moduleIsInstalled(name))
		uninstallModule(name);

	initializeModuleFromDir(path);
	
}


/*<< " <- NULL; gc(); return('succes!')";*/
