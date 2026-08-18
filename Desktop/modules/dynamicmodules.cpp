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



#include "log.h"
#include "dynamicmodules.h"
#include "dynamicmodules.h"
#include "utilities/qutils.h"
#include <QRegularExpression>
#include <QUrl>
#include <QUrlQuery>
#include <QDir>
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include "utilities/extractarchive.h"
#include "utilities/messageforwarder.h"
#include "engine/enginesync.h"
#include "modules/description/description.h"
#include "mainwindow.h"


#ifdef __APPLE__
#include "otoolstuff.h"
#include <filesystem>
#endif

DynamicModules * DynamicModules::_singleton = nullptr;

DynamicModules::DynamicModules(QObject *parent) : QObject(parent)
{
	if(_singleton) throw std::runtime_error("Can only instantiate DynamicModules once!");
	_singleton = this;

	_modulesInstallDirectory = AppDirs::userModulesDir().toStdWString() + L"/module_libs/";

	if(!std::filesystem::exists(_modulesInstallDirectory))
		std::filesystem::create_directories(_modulesInstallDirectory);
}

DynamicModules::~DynamicModules()
{
	_modules.clear(); //We do not need to delete them as they get DynamicModules as parent.

	_singleton = nullptr;
}


bool DynamicModules::initializeModuleFromDir(std::string moduleDir, bool bundled, bool isCommon)
{
	if(moduleDir.size() == 0)
		throw Modules::ModuleException("???", "Empty path was supplied to DynamicsModules::loadModule..");

	if(moduleDir[moduleDir.size() - 1] != '/')
		moduleDir += '/';

	Modules::DynamicModule	*newMod		= new Modules::DynamicModule(QString::fromStdString(moduleDir), this, bundled, isCommon);


	if(isCommon)
	{
		_commonModuleNames.insert(newMod->name());
		newMod->setIsCommon(true);
	}

	if(!initializeModule(newMod))
		return false;

	return true;
}

bool DynamicModules::initializeModule(Modules::DynamicModule * module)
{
	std::string	moduleName;

	try
	{
									moduleName				= module->name();
		Modules::DynamicModule	*	oldModule				= _modules.count(moduleName) > 0 && _modules[moduleName] != module ? _modules[moduleName] : nullptr;
		bool						wasAddedAlready			= true;

		if(std::count(_moduleNames.begin(), _moduleNames.end(), moduleName) == 0)
		{
			_moduleNames.push_back(moduleName);
			wasAddedAlready = false;
		}

		/* Fairly sure this isnt necessary anymore: if(oldModule) //I guess we could also check wasAddedAlready because I assume the only way oldModule can exist is if _moduleNames already contains moduleName.
		{
			emit stopEngines();					// Stop engines so that process will not try to work with Analyses while we are changing stuff...
		}*/

		_modules[moduleName] = module;
		
		
		if(!module->initialized())
		{
			connect(module, &Modules::DynamicModule::readyForUseChanged,			this,	&DynamicModules::loadedModulesChanged			);
			connect(module, &Modules::DynamicModule::titleChanged,					this,	&DynamicModules::loadedModulesChanged			);
			connect(module, &Modules::DynamicModule::descriptionReloaded,			this,	&DynamicModules::descriptionReloaded			);
			connect(module, &Modules::DynamicModule::statusChanged,					module,	[this, module, moduleName]()
			{
				if(module->status() == moduleStatus::error)
				{
						_moduleBundlesNeedingInstall.erase(moduleName);
						QTimer::singleShot(0, module, [this, moduleName](){ uninstallModule(moduleName); });
				}
			});
			
			module->initialize(MainWindow::singleton()->giveRootQmlContext());
		}

		if(!wasAddedAlready)
		{
			emit dynamicModuleAdded(module);
			emit loadModuleTranslationFile(module);
		}
		else if(oldModule)
		{
			emit storeAnalysesJson();
			unloadModule(moduleName);
			emit dynamicModuleReplaced(oldModule, module);
			delete oldModule;
			emit dynamicModuleChanged(module);
			emit loadModuleTranslationFile(module);
			emit reloadAnalysesJson();
		}		

		emit reloadQmlImportPaths();

		return true;
	}
	catch(Modules::ModuleException & e)		{ MessageForwarder::showWarning(tr("An error occured trying to initialize a module from dir %1, the error was: %2").arg(module->moduleRLibrary()).arg(e.what())); }
	catch(std::runtime_error & e)			{ MessageForwarder::showWarning(tr("An error occured trying to initialize a module from dir %1, the error was: %2").arg(module->moduleRLibrary()).arg(e.what())); }

	if(_modules.count(moduleName) > 0)
	{
		delete _modules[moduleName];
		_modules.erase(moduleName);

		for(size_t i = _moduleNames.size(); i > 0; i--)
			if(_moduleNames[i - 1] == moduleName)
				_moduleNames.erase(_moduleNames.begin() + i - 1);
	}

	return false;
}

std::string DynamicModules::loadModule(const std::string & moduleName)
{
	try
	{
		if(_modules.count(moduleName) == 0 && !initializeModuleFromDir(moduleDirectory(moduleName)))
			throw std::runtime_error("Couldn't load (and initialize) module " + moduleName);


		//DynamicModule	*loadMe	= _modules[moduleName];

		return moduleName;
	}
	catch(std::runtime_error & e)
	{
		MessageForwarder::showWarning(tr("An error occured trying to load module %1, the error was: '%2'").arg(tq(moduleName)).arg(e.what()));
		return "";
	}
}

void DynamicModules::unloadModule(const std::string & moduleName)
{
	Log::log() << "Module '" << moduleName << "' being registered for unloading!" << std::endl;

	_moduleBundlesNeedingInstall.erase(moduleName);

	if(_modules.count(moduleName) > 0)
	{
		Modules::DynamicModule * dynMod = _modules[moduleName];

		emit dynamicModuleUnloadBegin(dynMod);
		emit reloadQmlImportPaths();
	}
}


void DynamicModules::registerForInstalling(const std::string & moduleName)
{
	if(_moduleBundlesNeedingInstall.find(moduleName) == _moduleBundlesNeedingInstall.end())
	{
		Log::log() << "Bundle '" << moduleName << "' being registered for installing" << std::endl;
		_moduleBundlesNeedingInstall.insert(moduleName);
	}
}

void DynamicModules::registerForUninstall(const std::string &moduleName)
{
	if(_modulesNeedingRemoval.find(moduleName) == _modulesNeedingRemoval.end())
	{
		Log::log() << "Bundle '" << moduleName << "' being registered for removal" << std::endl;
		_modulesNeedingRemoval.insert(moduleName);
	}
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


void DynamicModules::replaceModule(Modules::DynamicModule * module)
{
	std::string moduleName = module->name();

	if(_modules[moduleName] == module)
		return;

	Modules::DynamicModule * oldModule = _modules[moduleName];

	_modules[moduleName] = module;

	emit unloadModule(moduleName);
	emit dynamicModuleReplaced(oldModule, module);
	emit dynamicModuleChanged(module);
	emit reloadQmlImportPaths();
	delete oldModule;
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

	bool	registerForDynamicUninstall		= true,
			replacedWithBundled = bundledModuleInFilesystem(moduleName);

	if(replacedWithBundled)
		initializeModuleFromDir(bundledModuleLibraryPath(moduleName), true, _commonModuleNames.count(moduleName) > 0);
	else if(_modules.count(moduleName) > 0)
	{
		unloadModule(moduleName);
		_modules[moduleName]->setInstalled(false);

		if(_modules[moduleName]->isBundled() || _modules[moduleName]->isLibpathDevMod())
			registerForDynamicUninstall = false;

		for(int i=int(_moduleNames.size()) - 1; i>=0; i--)
			if(_moduleNames[size_t(i)] == moduleName)
				_moduleNames.erase(_moduleNames.begin() + i);

		delete _modules[moduleName];
		_modules.erase(moduleName);
	}

	if(registerForDynamicUninstall)
		registerForUninstall(moduleName);

	if(!replacedWithBundled)	emit dynamicModuleUninstalled(QString::fromStdString(moduleName));

}

Modules::DynamicModule* DynamicModules::requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet)
{
	if(theSet.size() == 0)
		return nullptr;

	std::string installMe = *theSet.begin();
	theSet.erase(installMe);

	return _modules[installMe];
}


stringset DynamicModules::moduleBundlesNeedingInstall() const
{
	return _moduleBundlesNeedingInstall;
}

stringset DynamicModules::modulesNeedingUninstall() const
{
	return _modulesNeedingRemoval;
}

Json::Value	DynamicModules::getJsonForBundleInstallRequest()
{
	if(_moduleBundlesNeedingInstall.size() == 0)
		throw std::runtime_error("Tried to get json for ModuleBundle install request but there are none, getJsonForBundleInstallRequest should never have been called.");

#ifdef _WIN32
    const QString prefix = "file:///";
#else
    const QString prefix = "file://";
#endif

    QString list = "";
    for(auto& bundle : _moduleBundlesNeedingInstall) list += "'" + QString(bundle.c_str()).remove(prefix) + "'" + ",";
	list.removeLast();

	QString code = QString(
	R"readableR(
	tmp <- .libPaths();
	.libPaths("%1");
	library("jaspModuleBundleManager")
	bundles <- c(%2)
	f <- function(bundle) {jaspModuleBundleManager::installJaspModuleBundle(installPath="%3", bundlePath=bundle)}
	paste(sapply(bundles, f), collapse = ';')
	)readableR")
	.arg(AppDirs::bundledModulesDir() + "Tools/jaspModuleBundleManager_library/")
	.arg(list)
	.arg(AppDirs::userModulesDir());


	Json::Value requestJson(Json::objectValue);
	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::installNeeded);
	requestJson["moduleCode"]		= code.toStdString();
	requestJson["moduleName"]		= list.toStdString();

	return requestJson;

}

Json::Value DynamicModules::getJsonForModuleUninstallRequest()
{
	if(_modulesNeedingRemoval.size() == 0)
		throw std::runtime_error("Tried to get json for Module uninstall request but there are none,  getJsonForModuleUninstallRequest should never have been called.");

	QString list = "";
	for(auto& module : _modulesNeedingRemoval) list += "'" + QString(module.c_str()) + "'" + ",";
	list.removeLast();

	QString code = QString(
	R"readableR(
	tmp <- .libPaths();
	.libPaths("%1");
	library("jaspModuleBundleManager")
	bundles <- c(%2)
	f <- function(module) {jaspModuleBundleManager::uninstallJaspModuleBundle(installPath="%3", name=module)}
	paste(sapply(bundles, f), collapse = ';')
	)readableR")
					   .arg(AppDirs::bundledModulesDir() + "Tools/jaspModuleBundleManager_library/")
					   .arg(list)
					   .arg(AppDirs::userModulesDir());


	Json::Value requestJson(Json::objectValue);
	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::uninstallNeeded);
	requestJson["moduleCode"]		= code.toStdString();
	requestJson["moduleName"]		= list.toStdString();

	return requestJson;
}

Modules::DynamicModule *DynamicModules::dynamicModuleLowerCased(QString moduleName) const
{
	moduleName = moduleName.toLower(); //just enforce it

	for(const std::string & modName : _moduleNames)
		if(tq(modName).toLower() == moduleName)
		{
			return dynamicModule(modName);
		}

	return nullptr;
}


void DynamicModules::installationPackagesFailed(const QString & moduleName, const QString & errorMessage)
{
	if(_modules.count(moduleName.toStdString()) > 0)
		_modules[moduleName.toStdString()]->setInstallingSucces(false);

	uninstallModule(moduleName.toStdString());

	_moduleBundlesNeedingInstall.erase(moduleName.toStdString());

	if(moduleName.toStdString() == developmentModuleName())
		setDevelopersModuleInstallButtonEnabled(true);
	
	
	MessageForwarder::showWarning(
				tq("Installation of Module %1 failed").arg(moduleName),
				tr("The installation of Module %1 failed with the following errormessage:\n%2").arg(moduleName).arg(errorMessage));	
}

void DynamicModules::installationPackagesSucceeded(const QString & moduleNames)
{
	Log::log() << "Installing Bundles for modules (" << moduleNames.toStdString() << ") succeeded!" << std::endl;

	QString listStr = QString(moduleNames);
	QStringList modulesLibs =  listStr.split(';', Qt::SkipEmptyParts);

	for(QString& moduleLib : modulesLibs) {
		auto dynMod = initializeModuleFromDir(moduleLib.toStdString(), false, true);
	}
	_moduleBundlesNeedingInstall.clear();
    // MessageForwarder::showWarning(tr("Install complete"), tr("Completed installation of Bundles: ") + listStr);
}


void DynamicModules::unInstallationPackagesSucceeded(const QString &moduleNames)
{
	Log::log() << "Modules succesfully uninstalled" << std::endl;
	_modulesNeedingRemoval.clear();
}

void DynamicModules::unInstallationPackagesFailed(const QString &moduleName, const QString &errorMessage)
{
	_modulesNeedingRemoval.clear();
	MessageForwarder::showWarning(
		tq("Uninstall of Module %1 failed").arg(moduleName),
		tr("The removal of Module %1 failed with the following errormessage:\n%2").arg(moduleName).arg(errorMessage));
}


Modules::AnalysisEntry* DynamicModules::retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile)
{
	std::string moduleName		= jsonFromJaspFile.get("moduleName", "Modulename wasn't actually filled!").asString();

	if(_modules.count(moduleName) > 0)
		return _modules[moduleName]->retrieveCorrespondingAnalysisEntry(jsonFromJaspFile);

	throw Modules::ModuleException(moduleName,
		fq(tr("Module is not available, to load this JASP file properly you will need to install it first and then retry.\n"
		"If you do not have this module you can try our built-in module library, the module's website: \"%1\" or"
		", if that doesn't help, you could try our github issuetracker: https://github.com/jasp-stats/jasp-issues/issues."
	).arg(tq(jsonFromJaspFile.get("moduleWebsite", "jasp-stats.org").asString()))));
}

bool DynamicModules::isFileAnArchive(const QString &  filepath)
{
	return ExtractArchive::isFileAnArchive(filepath.toStdString());
}

void DynamicModules::uninstallJASPModule(const QString & moduleName)
{
	uninstallModule(moduleName.toStdString());
}

void DynamicModules::installJASPModule(const QString & moduleBundlePath)
{	
	registerForInstalling(moduleBundlePath.toStdString());

}

void DynamicModules::uninstallJASPDeveloperModule()
{
	if(_modules.count(developmentModuleName()))
		uninstallModule(developmentModuleName());
}

void DynamicModules::refreshDeveloperModule(bool R, bool Qml)
{
	if(_modules.count(developmentModuleName())) {
		EngineSync::singleton()->killModuleEngine(_modules[developmentModuleName()]);
		if(R && Qml)
			installJASPDeveloperModule();
		else if(R)
			emit dynamicModuleChanged(_modules[developmentModuleName()]);
		else if(Qml)
			emit dynamicModuleQmlChanged(_modules[developmentModuleName()]);
	}
}

void DynamicModules::installJASPDeveloperModule()
{
	bool	directLibpathEnabled	= Settings::value(Settings::DIRECT_LIBPATH_ENABLED).toBool();
	QString modulePath				= directLibpathEnabled ? QDir::cleanPath(Settings::value(Settings::DIRECT_LIBPATH_FOLDER).toString().trimmed()) : Settings::value(Settings::DEVELOPER_FOLDER).toString();

	if(modulePath == "")
	{
		MessageForwarder::showWarning(tr("Select a folder"), tr("To install a development module you need to select the folder you want to watch and load, you can do this under the filemenu, Preferences->Advanced."));
		return;
	}
	else if(!QDir(modulePath).exists())
	{
		Log::log() << modulePath << " doesnt seem to exist according to QDir, it gives: '" << QDir(modulePath).absolutePath() << "'." << std::endl;

		MessageForwarder::showWarning(tr("Select an exisiting folder"), tr("To install a development module you need to select and existing folder, you selected '%1' but it doesn't exist.").arg(modulePath));
		return;
	}

	setDevelopersModuleInstallButtonEnabled(false);

	try
	{
		emit storeAnalysesJson();

		QQmlContext * context = MainWindow::singleton()->giveRootQmlContext();
		Modules::DynamicModule * devMod = directLibpathEnabled ? new Modules::DynamicModule(this, context, modulePath) : new Modules::DynamicModule(this, context);

		std::string origin	= devMod->modulePackage(),
					name	= devMod->name(),
					dest	= devMod->moduleRLibrary().toStdString();

		if(moduleIsInstalledByUser(name))
		{
			uninstallModule(name);
		}
		else if(_modules.count(name) > 0 && _modules[name] != devMod)
			replaceModule(devMod);

		_modules[name] = devMod;
		if(directLibpathEnabled) {
			initializeModule(devMod);
		}
		
		emit reloadAnalysesJson();
	}
	catch(std::exception & e)
	{
		MessageForwarder::showWarning(tr("Problem initializing module"), tr("There was a problem loading the developer module:\n\n") + e.what());
		setDevelopersModuleInstallButtonEnabled(true);
		
		emit reloadAnalysesJson();
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
				if(entryinst.isFile() && Modules::DynamicModule::isDescriptionFile(entryinst.fileName()))
					descFound = entryinst.fileName();
				else if(entryinst.isDir())
				{
					QString dir = entryinst.fileName().toLower();

					if(dir == "qml")	qmlFound	= true;
					if(dir == "icons")	iconsFound	= true;
				}
		}
		else if(entry.isDir() && entry.fileName().toLower() == "qml")
			qmlFound = true;
		else if(entry.isDir() && entry.fileName().toLower() == "icons")
			iconsFound = true;
		else if(entry.isDir() && entry.fileName().toUpper() == "R")
			rFound = true;
		else if(entry.isFile() && Modules::DynamicModule::isDescriptionFile(entry.fileName()))
				descFound = entry.fileName();

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
	//QML is watched by Analysis itself
}

void DynamicModules::insertCommonModuleNames(std::set<std::string> commonModules) 
{ 
	for(const std::string & common : commonModules)
	{
		_commonModuleNames.insert(common); 
		
		if(dynamicModule(common))
			dynamicModule(common)->setIsCommon(true);
	}

}

void DynamicModules::clearCommonModules()
{
	_commonModuleNames.clear();
	
	for(auto & module : _modules)
		module.second->setIsCommon(false);
}

void DynamicModules::refreshCommonModules(const QStringList& overrideCommon)
{
	clearCommonModules();
	
	for(const std::string & modStr : fql(overrideCommon))
		if(dynamicModule(modStr))
		{
			_commonModuleNames.insert(modStr);
			dynamicModule(modStr)->setIsCommon(true);
		}
	
	RibbonModel::singleton()->setCommonOrder(overrideCommon);
}

///This function says it's copying something, and maybe it did that before, but it doesn't seem to be doing so now.
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

			this->_modules[this->developmentModuleName()]->reloadDescription(MainWindow::singleton()->giveRootQmlContext());
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

	connect(watcher, &QFileSystemWatcher::fileChanged, [&](const QString & path)
	{
		//If only a file changes then update this single file
		QFileInfo srcFileChanged(path);

		QFile	srcFile(path),
				dstFile(dst.filePath(folder != "help" ? srcFileChanged.fileName() : srcFileChanged.fileName().toLower()));

		if(folder.toUpper() == "R")
			this->regenerateDeveloperModuleRPackage();

		if(folder == "help")		emit	this->reloadHelpPage();
	});

	connect(watcher, &QFileSystemWatcher::directoryChanged, [&](QString path)
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
	if(devMod->isLibpathDevMod())
		emit dynamicModuleChanged(devMod);
}

QString DynamicModules::moduleDirectoryQ(const QString & moduleName)	const
{
	if(moduleName == tq(developmentModuleName()))	return developmentModuleFolder();
													return AppDirs::userModulesDir() + moduleName + '/';
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
	Modules::Description * desc = nullptr;

	try
	{
		desc = Modules::DynamicModule::instantiateDescriptionQml(MainWindow::singleton()->giveRootQmlContext(), tq(Modules::DynamicModule::getDescriptionQmlFromArchive(fq(archiveFilePath))), QUrl("Description.qml"), fq(QFileInfo(archiveFilePath).baseName()));
	}
	catch(Modules::ModuleException & e)
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
			.arg(desc->versionStr())
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
	return fq(AppDirs::bundledModulesLibDir()) + moduleName + "/";
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

const QStringList DynamicModules::loadedModules() const
{
	QStringList mods;

	for(const std::string & mod : _moduleNames)
		if(_modules.at(mod)->readyForUse())
			mods << tq(mod);

	return mods;
}

const QStringList DynamicModules::loadedModulesTitles() const
{
	QStringList mods;

	for(const std::string & mod : _moduleNames)
		if(_modules.at(mod)->readyForUse())
			mods << _modules.at(mod)->titleQ();

	return mods;
}

