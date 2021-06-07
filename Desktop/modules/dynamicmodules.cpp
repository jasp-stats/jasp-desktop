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
#include "gui/messageforwarder.h"
#include "utilities/appdirs.h"
#include "utilities/qutils.h"
#include "log.h"
#include "modules/description/description.h"
#include "modules/description/entrybase.h"
#include "modules/upgrader/upgrades.h"
#include "modules/upgrader/upgrade.h"
#include "modules/upgrader/changejs.h"
#include "modules/upgrader/changecopy.h"
#include "modules/upgrader/changeremove.h"
#include "modules/upgrader/changerename.h"
#include "modules/upgrader/changesetvalue.h"
#include "gui/preferencesmodel.h"
#include "dirs.h"
#include <QRegularExpression>

namespace Modules
{

DynamicModules * DynamicModules::_singleton = nullptr;

DynamicModules::DynamicModules(QObject *parent) : QObject(parent)
{
	if(_singleton) throw std::runtime_error("Can only instantiate DynamicModules once!");
	_singleton = this;

	connect(this, &DynamicModules::stopEngines, this, &DynamicModules::enginesStopped, Qt::QueuedConnection);

	_modulesInstallDirectory = AppDirs::userModulesDir().toStdWString();

	if(!boost::filesystem::exists(_modulesInstallDirectory))
		boost::filesystem::create_directories(_modulesInstallDirectory);
}

DynamicModules::~DynamicModules()
{
	_modules.clear(); //We do not need to delete them as they get DynamicModules as parent.

	_singleton = nullptr;
}

void DynamicModules::initializeInstalledModules()
{
	boost::system::error_code error;
	for (boost::filesystem::directory_iterator itr(_modulesInstallDirectory, error); !error && itr != boost::filesystem::directory_iterator(); itr++)
	{
		std::string path			= itr->path().generic_string(),
					name			= itr->path().filename().generic_string(),
					problem			= fq(tr("Initializing module during JASP startup failed, should the module be removed?"));
		bool		askForCleanup	= false;

		//Development Module should always be fresh!
		if(name == defaultDevelopmentModuleName())	
			boost::filesystem::remove_all(itr->path());
		
		else if(name.size() > 0 && name[0] != '.' && QFileInfo(tq(path)).isDir())	
			try
			{
				if(!initializeModuleFromDir(path))
					askForCleanup = true;
			}
			catch(ModuleException & modException)
			{
				askForCleanup = true;
				problem = fq(tr("Initializing module during JASP startup failed with the following message:\n%1\n\nShould the module be removed?").arg(tq(modException.problemDescription)));
			}
		
		if(askForCleanup && MessageForwarder::showYesNo(tr("Initializing module %1 failed").arg(tq(name)), tq(problem)))
			boost::filesystem::remove_all(itr->path());
	}
}

bool DynamicModules::initializeModuleFromDir(std::string moduleDir, bool bundled, bool isCommon)
{
	if(moduleDir.size() == 0)
		throw ModuleException("???", "Empty path was supplied to DynamicsModules::loadModule..");

	if(moduleDir[moduleDir.size() - 1] != '/')
		moduleDir += '/';

	DynamicModule	*newMod		= new DynamicModule(QString::fromStdString(moduleDir), this, bundled, isCommon);

	if(isCommon)
		_commonModuleNames.insert(newMod->name());

	if(!initializeModule(newMod))
		return false;

	if(isCommon)
	{
		if(_startingUp)	newMod->setReadyForUse();  //To make sure "engineSync->loadAllActiveModules()" loads it.
		else			newMod->setLoadingNeeded();
	}

	return true;
}

bool DynamicModules::initializeModule(DynamicModule * module)
{
	std::string	moduleName;

	try
	{
							moduleName				= module->name();
		DynamicModule	*	oldModule				= _modules.count(moduleName) > 0 && _modules[moduleName] != module ? _modules[moduleName] : nullptr;
		bool				wasAddedAlready			= true;

		if(std::count(_moduleNames.begin(), _moduleNames.end(), moduleName) == 0)
		{
			_moduleNames.push_back(moduleName);
			wasAddedAlready = false;
		}

		if(oldModule) //I guess we could also check wasAddedAlready because I assume the only way oldModule can exist is if _moduleNames already contains moduleName.
		{
			emit stopEngines();					// Stop engines so that process will not try to work with Analyses while we are changing stuff...
			_modulesToBeUnloaded.clear(); //if we are going to restart the engines we can also forget anything that's loaded and needs to be unloaded
		}

		_modules[moduleName] = module;
		
		
		if(!module->initialized())
		{	
			connect(module, &DynamicModule::registerForLoading,				this, &DynamicModules::registerForLoading			);
			connect(module, &DynamicModule::registerForInstalling,			this, &DynamicModules::registerForInstalling		);
			connect(module, &DynamicModule::registerForInstallingModPkg,	this, &DynamicModules::registerForInstallingModPkg	);
			connect(module, &DynamicModule::descriptionReloaded,			this, &DynamicModules::descriptionReloaded			);
			
			module->initialize();
		}

		if(!wasAddedAlready)
		{
			emit dynamicModuleAdded(module);
			emit loadModuleTranslationFile(module);
		}
		else if(oldModule)
		{
			emit dynamicModuleReplaced(oldModule, module);
			delete oldModule;
			emit dynamicModuleChanged(module);
			emit loadModuleTranslationFile(module);
			emit restartEngines();

		}		

		emit reloadQmlImportPaths();

		return true;
	}
	catch(ModuleException & e)		{ MessageForwarder::showWarning(tr("An error occured trying to initialize a module from dir %1, the error was: %2").arg(module->moduleRLibrary()).arg(e.what())); }
	catch(std::runtime_error & e)	{ MessageForwarder::showWarning(tr("An error occured trying to initialize a module from dir %1, the error was: %2").arg(module->moduleRLibrary()).arg(e.what())); }

	if(_modules.count(moduleName) > 0)
	{
		delete _modules[moduleName];
		_modules.erase(moduleName);

		for(size_t i = _moduleNames.size(); i > 0; i--)
			if(_moduleNames[i - 1] == moduleName)
				_moduleNames.erase(_moduleNames.begin() + i - 1);
	}

	emit restartEngines();

	return false;
}

std::string DynamicModules::loadModule(const std::string & moduleName)
{
	try
	{
		if(_modules.count(moduleName) == 0 && !initializeModuleFromDir(moduleDirectory(moduleName)))
			throw std::runtime_error("Couldn't load (and initialize) module " + moduleName);


		DynamicModule	*loadMe	= _modules[moduleName];

		if(_startingUp)	loadMe->setReadyForUse(); //So that engineSync->loadAllActiveModules will load this module
		else			loadMe->setLoadingNeeded();

		return moduleName;
	}
	catch(std::runtime_error & e)
	{
		MessageForwarder::showWarning(tr("An error occured trying to load module %1, the error was: '%2'").arg(tq(moduleName)).arg(e.what()));
		return "";
	}
}

void DynamicModules::registerForInstalling(const std::string & moduleName)
{
	registerForInstallingSubFunc(moduleName, false);
}

void DynamicModules::registerForInstallingModPkg(const std::string & moduleName)
{
	registerForInstallingSubFunc(moduleName, true);
}

void DynamicModules::stopAndRestartEngines()
{
	emit stopEngines();
	_modulesToBeUnloaded.clear(); //if we are going to restart the engines we can also forget anything that's loaded and needs to be unloaded
	emit restartEngines();
}

QStringList DynamicModules::importPaths() const
{
	QStringList allImportPaths;

	for(const auto & nameMod : _modules)
	{
		QDir moduleFolder(tq(nameMod.second->moduleInstFolder()));

		if(moduleFolder.exists("qmldir")) //So the inst contained a qmldir, and we give the library as an importpath
			allImportPaths.append(nameMod.second->moduleRLibrary());
	}

	return allImportPaths;
}

void DynamicModules::registerForInstallingSubFunc(const std::string & moduleName, bool onlyModPkg)
{
	Log::log() << "Module '" << moduleName << "' being registered for installing (onlyModPkg? " << (onlyModPkg ? "true" : "false") << ")!" << std::endl;

	_modulesToBeUnloaded.erase(moduleName);
	_modulesToBeLoaded.erase(moduleName);
	_modulesInstallPackagesNeeded[moduleName] = onlyModPkg;

	if(_modules[moduleName]->loaded())	//If the package is already loaded it might be hard to convince R to reinstall it properly, so let's restart the engines
	{
		stopAndRestartEngines();
		_modules[moduleName]->setUnloaded();
	}
}

void DynamicModules::registerForLoading(const std::string & moduleName)
{
	Log::log() << "Module '" << moduleName << "' being registered for loading!" << std::endl;

	if(_modulesInstallPackagesNeeded.count(moduleName) > 0)
		return; //When the install is done it will trigger the need for loading anyway.

	_modulesToBeUnloaded.erase(moduleName);

	//Ok, it might be the case that this module depends on some other module that isn't loaded yet.

	if(!requiredModulesForModuleReady(moduleName))
	{
		Log::log() << "Required modules for module '" << moduleName << "' not ready, so will not load it now." << std::endl;
		_modulesWaitingForDependency.insert(moduleName);
	}
	else
		_modulesToBeLoaded.insert(moduleName);
}

bool DynamicModules::requiredModulesForModuleReady(const std::string & moduleName) const
{
	DynamicModule	*	dynMod	= _modules.at(moduleName);
	std::set<std::string>		reqMods	= dynMod->importsR();

	for(const std::string & reqMod : reqMods)
		if(_modules.count(reqMod) > 0 && !_modules.at(reqMod)->readyForUse())
			return false;

	return true;
}

void DynamicModules::unloadModule(const std::string & moduleName)
{
	Log::log() << "Module '" << moduleName << "' being registered for unloading!" << std::endl;

	_modulesInstallPackagesNeeded	.erase(moduleName);
	_modulesToBeLoaded				.erase(moduleName);
	_modulesWaitingForDependency	.erase(moduleName);

	if(_modules.count(moduleName) > 0)
	{
		DynamicModule * dynMod		= _modules[moduleName];
		_modulesToBeUnloaded[moduleName]	= dynMod->requestJsonForPackageUnloadingRequest();

		dynMod->setUnloaded();

		emit dynamicModuleUnloadBegin(dynMod);
		emit reloadQmlImportPaths();
	}
}

void DynamicModules::replaceModule(DynamicModule * module)
{
	std::string moduleName = module->name();

	if(_modules[moduleName] == module)
		return;

	DynamicModule * oldModule = _modules[moduleName];

	emit stopEngines();					// Stop engines so that process will not try to work with Analyses while we are changing stuff...
	_modulesToBeUnloaded.clear(); //if we are going to restart the engines we can also forget anything that's loaded and needs to be unloaded

	_modules[moduleName] = module;

	//Tmp folder with pkg data was removed so lets unpackage (again)
	if(!module->isDevMod())
		module->unpackage();

	//registerForInstalling(moduleName);



	emit dynamicModuleReplaced(oldModule, module);
	emit dynamicModuleChanged(module);
	emit reloadQmlImportPaths();
	delete oldModule;

	emit restartEngines();
}

void DynamicModules::uninstallModule(const std::string & moduleName)
{
	if(moduleName == developmentModuleName())
	{
		delete _devModDescriptionWatcher;
		delete _devModRWatcher;

		_devModDescriptionWatcher	= nullptr;
		_devModRWatcher				= nullptr;
	}

	bool	removeFolder		= true,
			replacedWithBundled = bundledModuleInFilesystem(moduleName);

	if(replacedWithBundled)
		initializeModuleFromDir(bundledModuleLibraryPath(moduleName), true, _commonModuleNames.count(moduleName) > 0);
	else if(_modules.count(moduleName) > 0)
	{
		unloadModule(moduleName);
		_modules[moduleName]->setInstalled(false);

		if(_modules[moduleName]->isBundled())
			removeFolder = false;

		for(int i=int(_moduleNames.size()) - 1; i>=0; i--)
			if(_moduleNames[size_t(i)] == moduleName)
				_moduleNames.erase(_moduleNames.begin() + i);

		delete _modules[moduleName];
		_modules.erase(moduleName);
	}

	if(removeFolder)
		removeUninstalledModuleFolder(moduleName);

	if(!replacedWithBundled)	emit dynamicModuleUninstalled(QString::fromStdString(moduleName));

}

void DynamicModules::removeUninstalledModuleFolder(const std::string & moduleName, bool enginesStopped)
{
	Log::log() << "DynamicModules::removeUninstalledModuleFolder("<< moduleName << ", engines " << (enginesStopped ? "stopped" : "started") << ")" << std::endl;

	std::wstring modulePath	= moduleDirectoryW(moduleName);

	try
	{
		if(boost::filesystem::exists(modulePath))
			boost::filesystem::remove_all(modulePath); //Can fail because R might have a library from this folder still loaded. On Windows (and perhaps other OSs) these opened files can't be removed.

	}
	catch (boost::filesystem::filesystem_error & e)
	{
		if(enginesStopped)
			MessageForwarder::showWarning(tr("Something went wrong removing files for module %1 at path '%2' and the error was: %3").arg(tq(moduleName)).arg(tq(moduleDirectory(moduleName))).arg(e.what()));
		else
		{
			Log::log() << "Probably some library was still loaded in R... Let's stop the engines!" << std::endl;

			emit stopEngines();
			_modulesToBeUnloaded.clear(); //if we are going to restart the engines we can also forget anything that's loaded and needs to be unloaded
			removeUninstalledModuleFolder(moduleName, true);
			emit restartEngines();
		}

		return;
	}
}

DynamicModule* DynamicModules::requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet)
{
	if(theSet.size() == 0)
		return nullptr;

	std::string installMe = *theSet.begin();
	theSet.erase(installMe);

	return _modules[installMe];
}

Json::Value	DynamicModules::getJsonForPackageInstallationRequest()
{
	if(_modulesInstallPackagesNeeded.size() == 0)
		return nullptr;

	std::string installMe	= _modulesInstallPackagesNeeded.begin()->first;
	bool		onlyModPkg = _modulesInstallPackagesNeeded.begin()->second;

	_modulesInstallPackagesNeeded.erase(installMe);

	return _modules[installMe]->requestJsonForPackageInstallationRequest(onlyModPkg);
}

Json::Value DynamicModules::getJsonForPackageUnloadingRequest()
{
	std::string firstModule		= _modulesToBeUnloaded.begin()->first;
	Json::Value	unloadRequest	= _modulesToBeUnloaded[firstModule];

	_modulesToBeUnloaded.erase(firstModule);

	return unloadRequest;
}

void DynamicModules::installationPackagesFailed(const QString & moduleName, const QString & errorMessage)
{
	if(_modules.count(moduleName.toStdString()) > 0)
		_modules[moduleName.toStdString()]->setInstallingSucces(false);

	MessageForwarder::showWarning(
				tq("Installation of Module %1 failed").arg(moduleName),
				tr("The installation of Module %1 failed with the following errormessage:\n%2").arg(moduleName).arg(errorMessage));

	uninstallModule(moduleName.toStdString());

	if(moduleName.toStdString() == developmentModuleName())
		setDevelopersModuleInstallButtonEnabled(true);
}

void DynamicModules::installationPackagesSucceeded(const QString & moduleName)
{
	Log::log() << "Installing packages for module (" << moduleName.toStdString() << ") succeeded!" << std::endl;
	_modules[moduleName.toStdString()]->setInstallingSucces(true);

	auto *dynMod = _modules[moduleName.toStdString()];

	bool wasInitialized = dynMod->initialized();

	if(!wasInitialized)			initializeModule(dynMod);
	if(dynMod->initialized())	registerForLoading(moduleName.toStdString());


	if(dynMod->isDevMod())
	{
		if(wasInitialized)
			emit dynamicModuleChanged(dynMod);

		startWatchingDevelopersModule();
		setDevelopersModuleInstallButtonEnabled(true);
	}
	else
		emit dynamicModuleChanged(dynMod);

	emit reloadQmlImportPaths();
}


void DynamicModules::loadingFailed(const QString & moduleName, const QString & errorMessage)
{
	Log::log() << "Loading packages for module (" << moduleName.toStdString() << ") failed because of: " << errorMessage.toStdString() << std::endl;
	if(moduleName != "*")
	{
		_modules[moduleName.toStdString()]->setLoadingSucces(false);

		MessageForwarder::showWarning(
					tr("Loading packages for Module %1 failed").arg(moduleName),
					tr("Loading the packages of Module %1 failed with the following errormessage:\n%2").arg(moduleName).arg(errorMessage));
	}
	else
		for(const auto & nameModule : _modules)
			if(nameModule.second->readyForUse())
				nameModule.second->setLoaded(false);
}

void DynamicModules::loadingSucceeded(const QString & moduleName)
{
	Log::log() << "Loading packages for module (" << moduleName.toStdString() << ") succeeded!" << std::endl;

	if(moduleName != "*")
	{
		_modules[moduleName.toStdString()]->setLoadingSucces(true);

		std::set<std::string> removeThese;

		for(const std::string & modWait : _modulesWaitingForDependency)
			if(requiredModulesForModuleReady(modWait))
			{
				Log::log() << "Required modules for module '" << modWait << "' ready, so now registering it for loading!" << std::endl;
				removeThese.insert(modWait);
				_modulesToBeLoaded.insert(modWait);
			}

		for(const std::string & modWait : removeThese)
			_modulesWaitingForDependency.erase(modWait);
	}
	else
		for(const auto & nameModule : _modules)
			if(nameModule.second->readyForUse())
				nameModule.second->setLoaded(true);
}

Modules::AnalysisEntry* DynamicModules::retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile)
{
	std::string moduleName		= jsonFromJaspFile.get("moduleName", "Modulename wasn't actually filled!").asString();

	if(_modules.count(moduleName) > 0)
		return _modules[moduleName]->retrieveCorrespondingAnalysisEntry(jsonFromJaspFile);

	throw ModuleException(moduleName,
		"Module is not available, to load this JASP file properly you will need to install it first and then retry.\n"
		"If you do not have this module you can try the module's website: \""  + jsonFromJaspFile.get("moduleWebsite", "jasp-stats.org").asString()	 +  "\" or"
		", if that doesn't help, you could try to contact the module's maintainer: \"" + jsonFromJaspFile.get("moduleMaintainer", "the JASP team").asString() + "\"."
	);
}

bool DynamicModules::isFileAnArchive(const QString &  filepath)
{
	return ExtractArchive::isFileAnArchive(filepath.toStdString());
}

void DynamicModules::uninstallJASPModule(const QString & moduleName)
{
	uninstallModule(moduleName.toStdString());
}

void DynamicModules::checkAndWarnForLC_CTYPE_C() const
{
#ifdef _WIN32
	if(PreferencesModel::prefs()->setLC_CTYPE_C() && !pathIsSafeForR(AppDirs::userModulesDir()))
		MessageForwarder::showWarning(tr("Path cannot be handled by R"), tr("You are trying to install a module but because the path '%1' contains non-ascii characters (or a space) R might have some trouble installing there. If the installation of this module fails you can work around this by changing your username (or making a new username) that only contains ascii-characters and no spaces. Or you can disable \"LC_CTYPE-set-to-C\" under Preferences->Advanced->DeveloperMode."));
#endif
}

void DynamicModules::installJASPModule(const QString & moduleZipFilename)
{
	checkAndWarnForLC_CTYPE_C();
	
	if(!QFile(moduleZipFilename).exists())
	{
		MessageForwarder::showWarning(tr("Cannot install module because %1 does not exist.").arg(moduleZipFilename));
		return;
	}

	//Do not unpack yet! replaceModule might restart the engine which might clean up the tmp folder
	DynamicModule * dynMod = new DynamicModule(moduleZipFilename.toStdString(), this, false);

	std::string moduleName = dynMod->name();

	if(moduleName == defaultDevelopmentModuleName())
	{
		MessageForwarder::showWarning(tr(
			"Cannot install module because it is named '%1' and that name is reserved for installing the development module.\n"
			"Change the name (in DESCRIPTION and description.json) and try it again. "
			"If you are not the author of this module and do not know how to do this, contact: %2").arg(tq(defaultDevelopmentModuleName())).arg(tq(dynMod->author()))
		);
		delete dynMod;
		return;
	}

	if(moduleIsInstalledByUser(moduleName))
		uninstallModule(moduleName);

	auto modNameQ = QString::fromStdString(moduleName);
	if(!QDir(AppDirs::userModulesDir() + "/" + modNameQ).exists())
		QDir(AppDirs::userModulesDir()).mkdir(modNameQ);

	if(_modules.count(moduleName) > 0 && _modules[moduleName]->isBundled())
		replaceModule(dynMod);
	else
	{
		dynMod->unpackage();
		_modules[moduleName] = dynMod;
	}

	registerForInstalling(moduleName);

}

void DynamicModules::installJASPDeveloperModule()
{
	if(Settings::value(Settings::DEVELOPER_FOLDER).toString() == "")
	{
		MessageForwarder::showWarning(tr("Select a folder"), tr("To install a development module you need to select the folder you want to watch and load, you can do this under the filemenu, Preferences->Advanced."));
		return;
	}
	
	checkAndWarnForLC_CTYPE_C();

	setDevelopersModuleInstallButtonEnabled(false);

	try
	{

		_devModSourceDirectory = QDir(Settings::value(Settings::DEVELOPER_FOLDER).toString());

		DynamicModule * devMod = new DynamicModule(this);

		std::string origin	= devMod->modulePackage(),
					name	= devMod->name(),
					dest	= devMod->moduleRLibrary().toStdString();

		if(moduleIsInstalledByUser(name))
		{
			uninstallModule(name);
			emit stopEngines();
			_modulesToBeUnloaded.clear(); //if we are going to restart the engines we can also forget anything that's loaded and needs to be unloaded
			emit restartEngines();
		}
		else if(_modules.count(name) > 0 && _modules[name] != devMod)
			replaceModule(devMod);

		DynamicModule::developmentModuleFolderCreate();

		_modules[name] = devMod;

		registerForInstalling(name);
	}
	catch(ModuleException & e)
	{
		MessageForwarder::showWarning(tr("Problem initializing module"), tr("There was a problem loading the developer module:\n\n") + e.what());
		setDevelopersModuleInstallButtonEnabled(true);
	}
}

void DynamicModules::startWatchingDevelopersModule()
{
	std::string origin	= _devModSourceDirectory.absolutePath().toStdString(),
	            name	= developmentModuleName(),
	            dest	= moduleDirectory(name);

	bool	rFound		= false,
	        qmlFound	= false,
	        iconsFound	= false;

	QString descFound	= "";

	for(const QFileInfo & entry : _devModSourceDirectory.entryInfoList(QDir::Filter::Dirs | QDir::Filter::Files | QDir::Filter::NoDotAndDotDot))
		if(entry.isDir() && entry.fileName().toLower() == "inst")
		{
			QDir instDir(entry.absoluteFilePath());
			for(const QFileInfo & entryinst : instDir.entryInfoList(QDir::Filter::Dirs | QDir::Filter::Files | QDir::Filter::NoDotAndDotDot))
				if(entryinst.isFile() && DynamicModule::isDescriptionFile(entryinst.fileName()))
					descFound = entryinst.fileName();
				else if(entryinst.isDir())
				{
					QString dir = entryinst.fileName().toLower();

					if(dir == "qml")	qmlFound	= true;
					if(dir == "icons")	iconsFound	= true;
				}
		}
		else if(entry.isDir() && entry.fileName().toUpper() == "R")
			rFound = true;

	if(!(descFound != "" && rFound && qmlFound && iconsFound))
	{
		MessageForwarder::showWarning(tr("Missing files or folders"), tr("The selected folder cannot be installed as a developer module because it does not contain all the necessary files and folders.") + "\n" +
			(descFound != ""	? "" : (tr("Create a inst/Description.qml file.") + "\n")) +
			(rFound				? "" : (tr("Create a R directory containing your analysis code.") + "\n")) +
			(qmlFound			? "" : (tr("Create a inst/qml directory containing your optionsforms.") + "\n")) +
			(iconsFound			? "" : (tr("Create a inst/icons directory containing the icons for your ribbonbuttons.") + "\n")));
		return;
	}

	devModCopyDescription(descFound);
	devModWatchFolder("R",		_devModRWatcher);
	devModWatchFolder("help",	_devModHelpWatcher);
}

void DynamicModules::devModCopyDescription(QString filename)
{
	const QString descJson = "inst/" + filename;

	QFileInfo src(_devModSourceDirectory.filePath(descJson));
	QFileInfo dst(QString::fromStdString(moduleDirectory(developmentModuleName()) + developmentModuleName() + "/")  + descJson);

	if(!src.exists())
	{
		if(dst.exists())
			MessageForwarder::showWarning(tr("Missing %1").arg(descJson), tr("You seem to have removed %1 from your development module directory. Without it your module cannot work, make sure to put it back. For now your old %2 file will be kept.").arg(descJson).arg(descJson));
		else
		{
			MessageForwarder::showWarning(tr("Missing %1").arg(descJson), tr("You seem to have never had a %1 in your development module directory. Without it your module cannot work, make sure to create one. How you installed is a bit of a mystery and thus the development module shall be uninstalled now").arg(descJson));
			uninstallModule(developmentModuleName());
		}
		return;
	}

	QFile	srcFile(src.absoluteFilePath()),
			dstFile(dst.absoluteFilePath());

	delete _devModDescriptionWatcher;

	_devModDescriptionWatcher	= new QFileSystemWatcher({src.absoluteFilePath()}, this);

	connect(_devModDescriptionWatcher, &QFileSystemWatcher::fileChanged, [=](const QString & path)
	{
		if(path != src.absoluteFilePath())
			throw std::runtime_error("This watcher ought to watch just a single file! ("+src.absoluteFilePath().toStdString()+")");

		if(src.exists()) //file changed because it still exists
		{
			Log::log() << "Watched file " << descJson.toStdString() << " was modified." << std::endl;

			QFile	srcFileChanged(src.absoluteFilePath()),
					dstFileChanged(dst.absoluteFilePath());

			this->_modules[this->developmentModuleName()]->reloadDescription();
			this->regenerateDeveloperModuleRPackage();
		}
		else
		{
			MessageForwarder::showWarning(tr("%1 was removed!").arg(descJson), tr("You seem to have removed %1 but this file is required for your module to work. The development module is going to be uninstalled now.").arg(descJson));
			uninstallModule(developmentModuleName());
		}
	});
}

void DynamicModules::devModWatchFolder(QString folder, QFileSystemWatcher * & watcher)
{
	QString infix	= folder.toUpper() != "R" ? "inst/" : "",
			dstPath = QString::fromStdString(developmentModuleName()) + "/" + infix + folder;
	QDir	src		= _devModSourceDirectory.absoluteFilePath(infix + folder),
			modDir	= QString::fromStdString(moduleDirectory(developmentModuleName())),
			dst		= modDir.absoluteFilePath(dstPath);

	if(!src.exists())
	{
		if(folder != "help") //help is not really necessary
		{
			if(dst.exists())
				MessageForwarder::showWarning(tr("Missing folder %1").arg(folder), tr("You seem to have removed the folder %1 from your development module directory. Without it your module cannot work, make sure to put it back. For now your old folder will be kept.").arg(folder));
			else
			{
				MessageForwarder::showWarning(tr("Missing folder %1").arg(folder), tr("You seem to have never had the folder %1 in your development module directory. Without it your module cannot work, make sure to create one. How you installed is a bit of a mystery and thus the development module shall be uninstalled now").arg(folder));
				uninstallModule(developmentModuleName());
			}
		}
		return;
	}

	delete watcher;
	watcher = new QFileSystemWatcher({_devModSourceDirectory.absoluteFilePath(folder)}, this);

	static const std::map<std::string, std::set<std::string>> _acceptedFilesInFolders = {{"", {"json"}}, {"r", {"r"}}, {"qml", {"qml"}}, {"icons", {"svg", "png", "ico", "jpg", "gif"}}, {"help", {"md", "html"}}};

	QStringList extensionFilter;
	for(const std::string & extension :  _acceptedFilesInFolders.at(folder.toLower().toStdString()))
		extensionFilter << "*." + QString::fromStdString(extension) << "*." + QString::fromStdString(extension).toUpper();

	std::set<QString> filesInSource;

	for(QFileInfo entry : src.entryInfoList(extensionFilter, QDir::Filter::Files))
	{
		QFile	srcFile(entry.absoluteFilePath()),
				dstFile(dst.filePath(folder != "help" ? entry.fileName() : entry.fileName().toLower()));

		filesInSource.insert(dstFile.fileName());

		watcher->addPath(entry.absoluteFilePath());
	}

	if(folder.toUpper() != "R")
		for(QFileInfo entry : dst.entryInfoList(QDir::Filter::Files))
		{
			QFile dstFile(entry.absoluteFilePath());

			if(filesInSource.count(dstFile.fileName()) == 0)
				dstFile.remove();
		}

	connect(watcher, &QFileSystemWatcher::fileChanged, [=](const QString & path)
	{
		//If only a file changes then update this single file
		QFileInfo srcFileChanged(path);

		QFile	srcFile(path),
				dstFile(dst.filePath(folder != "help" ? srcFileChanged.fileName() : srcFileChanged.fileName().toLower()));

		if(folder.toUpper() == "R")
			this->regenerateDeveloperModuleRPackage();

		if(folder == "help")		emit	this->reloadHelpPage();
	});

	connect(watcher, &QFileSystemWatcher::directoryChanged, [=, &watcher](QString path)
	{
		Log::log() << "Watched folder " << folder.toStdString() << " had a changed directory (file added or removed) on path: " << path.toStdString() << std::endl;

		if(folder.toUpper() == "R")
			this->regenerateDeveloperModuleRPackage();

		if(folder == "help")			emit	this->reloadHelpPage();
	});
}

void DynamicModules::regenerateDeveloperModuleRPackage()
{
	if(_modules.count(developmentModuleName()) == 0)
		throw std::runtime_error("void DynamicModules::regenerateDeveloperModuleRPackage() called but the development module is not initialized...");

	auto * devMod = _modules[developmentModuleName()];
	devMod->setStatus(moduleStatus::installModPkgNeeded);
}

Json::Value	DynamicModules::getJsonForReloadingActiveModules()
{
	Json::Value requestJson(Json::objectValue);

	std::stringstream loadingCode;
	for(size_t i=0; i<_moduleNames.size(); i++)
	{
		const std::string & modName = _moduleNames[i];

		if(_modules[modName]->readyForUse()) //should be loaded again
			loadingCode << _modules[modName]->generateModuleLoadingR(i == _moduleNames.size() - 1) << "\n";
	}

	if(loadingCode.str() != "")
		loadingCode << "return('" << DynamicModule::succesResultString() << "')"; //We could also avoid calling getJsonForReloadingActiveModules altogether but whatever

	requestJson["moduleCode"]		= loadingCode.str();
	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::loadingNeeded);
	requestJson["moduleName"]		= "*";

	return requestJson;
}

void DynamicModules::enginesStopped()
{
	//for(auto & nameMod : _modules)
	//	nameMod.second->setLoaded(false); //Cause we restarted the engines
	// It is slightly misleading all this, but by not setting them to unloaded the GUI becomes cleaner and we do not have to set them back to loaded later in a hacky fashion
}

QString DynamicModules::moduleDirectoryQ(const QString & moduleName)	const
{
	if(moduleName == tq(developmentModuleName()))	return developmentModuleFolder();
													return AppDirs::userModulesDir() + moduleName + '/';
}

bool DynamicModules::moduleHasUpgradesToApply(const std::string & module, const std::string & function, const Version & version)
{
	return _modules.count(module)> 0 && _modules[module]->hasUpgradesToApply(function, version);
}

void DynamicModules::applyUpgrade(const std::string & module, const std::string & function, const Version & version, Json::Value & analysesJson, UpgradeMsgs & msgs, StepsTaken & stepsTaken)
{
	return _modules[module]->applyUpgrade(function, version, analysesJson, msgs, stepsTaken);
}

std::string DynamicModules::moduleDirectory(const std::string & moduleName)	const
{
	return moduleDirectoryQ(tq(moduleName)).toStdString();
}

std::wstring DynamicModules::moduleDirectoryW(const std::string & moduleName)	const
{
	return moduleDirectoryQ(tq(moduleName)).toStdWString();
}

void DynamicModules::setDevelopersModuleInstallButtonEnabled(bool developersModuleInstallButtonEnabled)
{
	if (_devModInstallButtonOn == developersModuleInstallButtonEnabled)
		return;

	_devModInstallButtonOn = developersModuleInstallButtonEnabled;
	emit developersModuleInstallButtonEnabledChanged(_devModInstallButtonOn);
}

QString DynamicModules::getDescriptionFormattedFromArchive(QString archiveFilePath)
{
	Description * desc = nullptr;

	try
	{
		desc = DynamicModule::instantiateDescriptionQml(tq(DynamicModule::getDescriptionQmlFromArchive(fq(archiveFilePath))), QUrl("Description.qml"), fq(QFileInfo(archiveFilePath).baseName()));
	}
	catch(ModuleException & e)
	{
		MessageForwarder::showWarning(tr("Loading module description encountered a problem"), e.what());
		return "";
	}

	if(!desc)
	{
		MessageForwarder::showWarning(tr("Loading module description encountered a problem"), tr("<i>Could not load the description of the module in archive: '%1'</i>").arg(archiveFilePath));
		return "";
	}

	QString formattedDescription = tr(
				"<h3>%1</h3><i>Version %2</i>"											"<br>"
				"<p>%3</p>"																"<br><br>"
				"<i>Created by %4 and maintained by %5.</i>"							"<br>"
				"<i>See website for further details: <a href=\"http://%6\">%6</a></i>"	"<br>"
			  )
			.arg(desc->title())
			.arg(desc->version())
			.arg(desc->description())
			.arg(desc->author())
			.arg(desc->maintainer())
			.arg(desc->website().toString());

	delete desc;

	return formattedDescription;
}

void DynamicModules::setDataLoaded(bool dataLoaded)
{
	if (_dataLoaded == dataLoaded)
		return;

	_dataLoaded = dataLoaded;
	emit dataLoadedChanged(_dataLoaded);
}

bool DynamicModules::bundledModuleInFilesystem(const std::string & moduleName)
{
	return QDir(tq(bundledModuleLibraryPath(moduleName))).exists();
}

std::string DynamicModules::bundledModuleLibraryPath(const std::string & moduleName)
{
	return fq(AppDirs::bundledModulesDir()) + moduleName + "/";
}

QStringList DynamicModules::requiredModulesLibPaths(QString moduleName)
{
	QStringList returnThis;

	std::set<std::string> requiredModules = _modules[moduleName.toStdString()]->importsR();
	
	Log::log() << "DynamicModules::requiredModulesLibPaths(" << moduleName << ") sees the following R-pkgs: ";
	
	for(const std::string & reqMod : requiredModules)
		Log::log(false) << "'" << reqMod << "' ";
	Log::log(false) << std::endl;
	

	for(const std::string & reqMod : requiredModules)
		if(_modules.count(reqMod) > 0)
			returnThis.append(_modules[reqMod]->moduleRLibrary());

	return returnThis;
}

void DynamicModules::registerQMLTypes()
{
	qmlRegisterType<Modules::Description>						("JASP.Module", 1, 0, "Description"						);
	qmlRegisterType<Modules::AnalysisItem>						("JASP.Module", 1, 0, "Analysis"						);
	qmlRegisterType<Modules::Separator>							("JASP.Module", 1, 0, "Separator"						);
	qmlRegisterType<Modules::GroupTitle>						("JASP.Module", 1, 0, "GroupTitle"						);
	qmlRegisterType<Modules::Upgrades>							("JASP.Module", 1, 0, "Upgrades"						);
	qmlRegisterType<Modules::Upgrade>							("JASP.Module", 1, 0, "Upgrade"							);
	qmlRegisterType<Modules::ChangeJS>							("JASP.Module", 1, 0, "ChangeJS"						);
	qmlRegisterType<Modules::ChangeCopy>						("JASP.Module", 1, 0, "ChangeCopy"						);
	qmlRegisterType<Modules::ChangeRename>						("JASP.Module", 1, 0, "ChangeRename"					);
	qmlRegisterType<Modules::ChangeRemove>						("JASP.Module", 1, 0, "ChangeRemove"					);
	qmlRegisterType<Modules::ChangeSetValue>					("JASP.Module", 1, 0, "ChangeSetValue"					);
	qmlRegisterUncreatableType<Modules::EntryBase>				("JASP.Module", 1, 0, "EntryBase",						"Superclass for menu entries, shouldn't be instantiated manually");
	qmlRegisterUncreatableType<Modules::DynamicModule>			("JASP.Module", 1, 0, "DynamicModule",					"Can only be instantiated by JASP");
	qmlRegisterUncreatableType<Modules::DescriptionChildBase>	("JASP.Module", 1, 0, "DescriptionChildBase",			"Superclass for Description info, shouldn't be instantiated manually");
}

}
