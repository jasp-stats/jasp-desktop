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


DynamicModules * DynamicModules::_singleton = nullptr;

DynamicModules::DynamicModules(QObject *parent) : QObject(parent)
{
	if(_singleton) throw std::runtime_error("Can only instantiate DynamicModules once!");
	_singleton = this;

	connect(this, &DynamicModules::stopEngines, this, &DynamicModules::enginesStopped, Qt::QueuedConnection);

	_modulesInstallDirectory = AppDirs::modulesDir().toStdWString();

	if(!boost::filesystem::exists(_modulesInstallDirectory))
		boost::filesystem::create_directories(_modulesInstallDirectory);

	initializeInstalledModules();
}

DynamicModules::~DynamicModules()
{
	for(auto dynamic : _modules)
		delete dynamic.second;
	_modules.clear();
}

void DynamicModules::initializeInstalledModules()
{
	boost::system::error_code error;
	for (boost::filesystem::directory_iterator itr(_modulesInstallDirectory, error); !error && itr != boost::filesystem::directory_iterator(); itr++)
	{
		std::string path = itr->path().generic_string();
		std::string name = itr->path().filename().generic_string();

		//Development Module should always be fresh!
		if(name == defaultDevelopmentModuleName())	boost::filesystem::remove_all(itr->path());
		else if(name.size() > 0 && name[0] != '.')	initializeModuleFromDir(path);
	}
}

bool DynamicModules::initializeModuleFromDir(std::string moduleDir)
{
	if(moduleDir.size() == 0)
		throw std::runtime_error("Empty path was supplied to DynamicsModules::loadModule..");

	if(moduleDir[moduleDir.size() - 1] != '/')
		moduleDir += '/';

	Modules::DynamicModule	*newMod		= new Modules::DynamicModule(QString::fromStdString(moduleDir), this);

	return initializeModule(newMod);
}

bool DynamicModules::initializeModule(Modules::DynamicModule * module)
{
	try
	{
		std::string	moduleName				= module->name();
		bool		wasAddedAlready			= true;
					_modules[moduleName]	= module;

		if(std::count(_moduleNames.begin(), _moduleNames.end(), moduleName) == 0)
		{
			_moduleNames.push_back(moduleName);
			wasAddedAlready = false;
		}


		if(!module->initialized())
		{
			connect(module, &Modules::DynamicModule::registerForLoading,			this, &DynamicModules::registerForLoading);
			connect(module, &Modules::DynamicModule::registerForInstalling,			this, &DynamicModules::registerForInstalling);
			connect(module, &Modules::DynamicModule::registerForInstallingModPkg,	this, &DynamicModules::registerForInstallingModPkg);
			connect(module, &Modules::DynamicModule::descriptionReloaded,			this, &DynamicModules::descriptionReloaded);

			module->initialize();
		}

		if(!wasAddedAlready)
		{
			emit dynamicModuleAdded(module);
			emit loadModuleTranslationFile(module);
		}

		return true;
	}
	catch(std::runtime_error & e)
	{
		MessageForwarder::showWarning(tr("An error occured trying to initialize a module from dir %1, the error was: %2").arg(module->moduleRLibrary()).arg(e.what()));
		return false;
	}


}

std::string DynamicModules::loadModule(const std::string & moduleName)
{
	try
	{
		if(_modules.count(moduleName) == 0 && !initializeModuleFromDir(moduleDirectory(moduleName)))
			throw std::runtime_error("Couldn't load (and initialize) module " + moduleName);


		Modules::DynamicModule	*loadMe	= _modules[moduleName];
		loadMe->setLoadingNeeded();

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


void DynamicModules::registerForInstallingSubFunc(const std::string & moduleName, bool onlyModPkg)
{
	_modulesToBeUnloaded.erase(moduleName);
	_modulesToBeLoaded.erase(moduleName);
	_modulesInstallPackagesNeeded[moduleName] = onlyModPkg;

	if(_modules[moduleName]->loaded())	//If the package is already loaded it might be hard to convince R to reinstall it properly, so let's restart the engines
	{
		stopEngines();
		_modulesToBeUnloaded.clear(); //if we are going to restart the engines we can also forget anything that's loaded and needs to be unloaded
		restartEngines();

		_modules[moduleName]->setUnloaded();
	}
}

void DynamicModules::registerForLoading(const std::string & moduleName)
{
	if(_modulesInstallPackagesNeeded.count(moduleName) > 0)
		return; //When the install is done it will trigger the need for loading anyway.

	_modulesToBeUnloaded.erase(moduleName);
	_modulesToBeLoaded.insert(moduleName);
}

void DynamicModules::unloadModule(const std::string & moduleName)
{
	_modulesInstallPackagesNeeded.erase(moduleName);
	_modulesToBeLoaded.erase(moduleName);

	if(_modules.count(moduleName) > 0)
	{
		Modules::DynamicModule * dynMod		= _modules[moduleName];
		_modulesToBeUnloaded[moduleName]	= dynMod->requestJsonForPackageUnloadingRequest();

		dynMod->setUnloaded();

		emit dynamicModuleUnloadBegin(dynMod);
	}
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

	if(_modules.count(moduleName) > 0)
	{
		unloadModule(moduleName);
		_modules[moduleName]->setInstalled(false);

		for(int i=int(_moduleNames.size()) - 1; i>=0; i--)
			if(_moduleNames[size_t(i)] == moduleName)
				_moduleNames.erase(_moduleNames.begin() + i);

		delete _modules[moduleName];
		_modules.erase(moduleName);
	}

	removeUninstalledModuleFolder(moduleName);
}

void DynamicModules::removeUninstalledModuleFolder(const std::string & moduleName, bool enginesStopped)
{
	Log::log() << "DynamicModules::removeUninstalledModuleFolder("<< moduleName << ", engines " << (enginesStopped ? "stopped" : "started") << ")" << std::endl;

	std::wstring modulePath	= moduleDirectoryW(moduleName);

	try
	{
		if(boost::filesystem::exists(modulePath))
			boost::filesystem::remove_all(modulePath); //Can fail because R might have a library from this folder still loaded. On Windows (and perhaps other OSs) these openend files can't be removed.

	}
	catch (boost::filesystem::filesystem_error & e)
	{
		if(enginesStopped)
			MessageForwarder::showWarning(tr("Something went wrong removing files for module %1 at path '%2' and the error was: %3").arg(tq(moduleName)).arg(tq(moduleDirectory(moduleName))).arg(e.what()));
		else
		{
			Log::log() << "Probably some library was still loaded in R... Let's stop the engines!" << std::endl;

			stopEngines();
			_modulesToBeUnloaded.clear(); //if we are going to restart the engines we can also forget anything that's loaded and needs to be unloaded
			removeUninstalledModuleFolder(moduleName, true);
			restartEngines();
		}

		return;
	}

	emit dynamicModuleUninstalled(QString::fromStdString(moduleName));
}

Modules::DynamicModule* DynamicModules::requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet)
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

	MessageForwarder::showWarning(tq("Installation of Module %1 failed").arg(moduleName), tr("The installation of Module %1 failed with the following errormessage:\n%2").arg(moduleName).arg(errorMessage));

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
}


void DynamicModules::loadingFailed(const QString & moduleName, const QString & errorMessage)
{
	Log::log() << "Loading packages for module (" << moduleName.toStdString() << ") failed because of: " << errorMessage.toStdString() << std::endl;
	if(moduleName != "*")
	{
		_modules[moduleName.toStdString()]->setLoadingSucces(false);

		MessageForwarder::showWarning(tr("Loading packages for Module %1 failed").arg(moduleName), tr("Loading the packages of Module %1 failed with the following errormessage:\n%2").arg(moduleName).arg(errorMessage));
	}
}

void DynamicModules::loadingSucceeded(const QString & moduleName)
{
	Log::log() << "Loading packages for module (" << moduleName.toStdString() << ") succeeded!" << std::endl;

	if(moduleName != "*")
		_modules[moduleName.toStdString()]->setLoadingSucces(true);
}

Modules::AnalysisEntry* DynamicModules::retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile)
{
	std::string moduleName		= jsonFromJaspFile.get("moduleName", "Modulename wasn't actually filled!").asString();

	if(_modules.count(moduleName) > 0)
		return _modules[moduleName]->retrieveCorrespondingAnalysisEntry(jsonFromJaspFile);

	throw Modules::ModuleException(moduleName, "Module is not available, to load this JASP file properly you will need to install it first and then retry.\nIf you do not have this module you can try the module's website: \""  + jsonFromJaspFile.get("moduleWebsite", "jasp-stats.org").asString()	 +  "\" or, if that doesn't help, you could try to contact the module's maintainer: \"" + jsonFromJaspFile.get("moduleMaintainer", "the JASP team").asString() + "\".");
}

bool DynamicModules::isFileAnArchive(const QString &  filepath)
{
	return ExtractArchive::isFileAnArchive(filepath.toStdString());
}

void DynamicModules::uninstallJASPModule(const QString & moduleName)
{
	uninstallModule(moduleName.toStdString());
}

void DynamicModules::installJASPModule(const QString & moduleZipFilename)
{
	if(!QFile(moduleZipFilename).exists())
	{
		MessageForwarder::showWarning(tr("Cannot install module because %1 does not exist.").arg(moduleZipFilename));
		return;
	}

	Modules::DynamicModule * dynMod = new Modules::DynamicModule(moduleZipFilename.toStdString(), this);

	std::string moduleName = dynMod->name();

	if(moduleName == defaultDevelopmentModuleName())
	{
		MessageForwarder::showWarning(tr("Cannot install module because it is named '%1' and that name is reserved for installing the development module.\nChange the name (in DESCRIPTION and description.json) and try it again. If you are not the author of this module and do not know how to do this, contact: %2").arg(tq(defaultDevelopmentModuleName())).arg(tq(dynMod->author())));
		delete dynMod;
		return;
	}

	if(moduleIsInstalled(moduleName))
		uninstallModule(moduleName);

	auto modNameQ = QString::fromStdString(moduleName);
	if(!QDir(AppDirs::modulesDir() + "/" + modNameQ).exists())
		QDir(AppDirs::modulesDir()).mkdir(modNameQ);

	_modules[moduleName] = dynMod;

	registerForInstalling(moduleName);

}

void DynamicModules::installJASPDeveloperModule()
{
	if(Settings::value(Settings::DEVELOPER_FOLDER).toString() == "")
	{
		MessageForwarder::showWarning(tr("Select a folder"), tr("To install a development module you need to select the folder you want to watch and load, you can do this under the filemenu, Preferences->Advanced."));
		return;
	}

	setDevelopersModuleInstallButtonEnabled(false);

	_devModSourceDirectory = QDir(Settings::value(Settings::DEVELOPER_FOLDER).toString());

	Modules::DynamicModule * devMod = new Modules::DynamicModule(this);

	std::string origin	= devMod->modulePackage(),
				name	= devMod->name(),
				dest	= devMod->moduleRLibrary().toStdString();

	if(moduleIsInstalled(name))
	{
		uninstallModule(name);

		stopEngines();
		_modulesToBeUnloaded.clear(); //if we are going to restart the engines we can also forget anything that's loaded and needs to be unloaded
		restartEngines();
	}

	Modules::DynamicModule::developmentModuleFolderCreate();

	_modules[name] = devMod;

	registerForInstalling(name);
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
				if(entryinst.isFile() && Modules::DynamicModule::isDescriptionFile(entryinst.fileName()))
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
		    (descFound != ""	? "" : (tr("Create a inst/description.json file.") + "\n")) +
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
	for(int i=0; i<_moduleNames.size(); i++)
	{
		const std::string & modName = _moduleNames[i];

		if(_modules[modName]->readyForUse()) //should be loaded again
			loadingCode << _modules[modName]->generateModuleLoadingR(i == _moduleNames.size() - 1) << "\n";
	}

	if(loadingCode.str() == "")
		loadingCode << "return('" << Modules::DynamicModule::succesResultString() << "')"; //We could also avoid calling getJsonForReloadingActiveModules altogether but whatever

	requestJson["moduleCode"]		= loadingCode.str();
	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::loadingNeeded);
	requestJson["moduleName"]		= "*";

	return requestJson;
}

void DynamicModules::enginesStopped()
{
	for(auto & nameMod : _modules)
		nameMod.second->setLoaded(false); //Cause we restarted the engines
}

std::string DynamicModules::moduleDirectory(const std::string & moduleName)	const
{
	if(moduleName == developmentModuleName()) return developmentModuleFolder().toStdString();
	return AppDirs::modulesDir().toStdString() + moduleName + '/';
}

std::wstring DynamicModules::moduleDirectoryW(const std::string & moduleName)	const
{
	if(moduleName == developmentModuleName()) return developmentModuleFolder().toStdWString();
	return AppDirs::modulesDir().toStdWString() + QString::fromStdString(moduleName).toStdWString() + L'/';
}

void DynamicModules::setDevelopersModuleInstallButtonEnabled(bool developersModuleInstallButtonEnabled)
{
	if (_developersModuleInstallButtonEnabled == developersModuleInstallButtonEnabled)
		return;

	_developersModuleInstallButtonEnabled = developersModuleInstallButtonEnabled;
	emit developersModuleInstallButtonEnabledChanged(_developersModuleInstallButtonEnabled);
}

QString DynamicModules::getDescriptionJsonFromArchive(QString archiveFilePath)
{
	std::string description = Modules::DynamicModule::getDescriptionJsonFromArchive(archiveFilePath.toStdString());

	if(description == "") return "";

	//The javascript toJSON is more picky then JsonCPP so we do something seemoingly crazy here, transforming the text into json and back to text, but this removes "comments" (they are not officially allowed in JSON but jsoncpp does not care and javscript crashes on it)

	Json::Value json;
	Json::Reader().parse(description, json);

	return tq(json.toStyledString());
}

void DynamicModules::setDataLoaded(bool dataLoaded)
{
	if (_dataLoaded == dataLoaded)
		return;

	_dataLoaded = dataLoaded;
	emit dataLoadedChanged(_dataLoaded);
}
