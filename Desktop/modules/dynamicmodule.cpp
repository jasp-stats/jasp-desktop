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
#include <locale>
#include <QThread>
#include <iostream>
#include "tempfiles.h"
#include <QQmlContext>
#include <QQmlIncubator>
#include "dynamicmodule.h"
#include "dynamicmodules.h"
#include <QRegularExpression>
#include "utilities/qutils.h"
#include "upgrader/upgrades.h"
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include "utilities/messageforwarder.h"
#include "utilities/languagemodel.h"
#include "description/description.h"
#include "utilities/extractarchive.h"
#include "utilities/qmlutils.h"
#include "mainwindow.h"

#ifdef __APPLE__
#include "otoolstuff.h"
#include <filesystem>
#endif

namespace Modules
{

ModuleException::ModuleException(std::string moduleName, std::string problemDescription)
	: std::runtime_error("Module \"" + moduleName + "\" had a problem: " + problemDescription), moduleName(moduleName), problemDescription(problemDescription)
{}

const char * standardRIndent = "  ";

std::string DynamicModule::_developmentModuleName = "?";

///This constructor takes the path to an installed jasp-module as path (aka a directory that contains a library of R packages, one of which is the actual module with QML etc)
DynamicModule::DynamicModule(QString moduleDirectory, QObject *parent, bool bundled, bool isCommon)
	: QObject(parent), _moduleFolder(moduleDirectory), _bundled(bundled), _isCommon(isCommon)
{
	QDir moduleDir(_moduleFolder.absoluteDir());
	if(!moduleDir.exists())
		throw ModuleException("???", "Module folder '" + moduleDir.absolutePath().toStdString() + "' does not exist so cannot load from there!");

	_name		= stringUtils::stripNonAlphaNum(moduleDir.dirName().toStdString());

	setInstalled(true);
}

///This constructor takes the path to an R-package as first argument, this R-package must also be a jasp-module and will be installed to the app-directory for the particular OS it runs on.
DynamicModule::DynamicModule(std::string modulePackageFile, QObject *parent, bool unpack) : QObject(parent), _modulePackage(modulePackageFile)
{
	_name			= extractPackageNameFromArchive(modulePackageFile);
	_moduleFolder	= QFileInfo(AppDirs::userModulesDir() + QString::fromStdString(_name) + "/");

	loadDescriptionFromArchive(_modulePackage);
}

QFileInfo DynamicModule::developmentModuleFolder()
{
	return QFileInfo(AppDirs::userModulesDir() + QString::fromStdString(defaultDevelopmentModuleName()) + "/");
}

void DynamicModule::developmentModuleFolderCreate()
{
	if(developmentModuleFolder().dir().exists()) return;

	QDir(AppDirs::userModulesDir()).mkpath(QString::fromStdString(defaultDevelopmentModuleName()));
}

///This constructor is meant specifically for the development module and only *it*!
DynamicModule::DynamicModule(QObject * parent) : QObject(parent), _isDeveloperMod(true)
{
	_modulePackage	= Settings::value(Settings::DEVELOPER_FOLDER).toString().toStdString();
	_moduleFolder	= developmentModuleFolder();

					_name = extractPackageNameFromFolder(_modulePackage);
	if(_name == "") _name = defaultDevelopmentModuleName();

	Log::log() << "Development Module is constructed with name: '" << _name << "' and will be installed to '" << _moduleFolder.absoluteFilePath().toStdString() << "' from source dir: '" << _modulePackage << "'" << std::endl;

	_developmentModuleName = _name;

	loadDescriptionFromFolder(_modulePackage);
}


///This constructor is meant specifically for the development module from a libpath *it*!
DynamicModule::DynamicModule(QObject * parent, QString libpath) : QObject(parent), _isDeveloperMod(true), _isLibpathDevMod(true)
{
	libpath = patchLibPathHelperFunc(libpath);
	_modulePackage	= fq(libpath + "/" + Settings::value(Settings::DIRECT_DEVMOD_NAME).toString() + "/");
	_moduleFolder	= QFileInfo(libpath + "/");
	_name = extractPackageNameFromFolder(_modulePackage);

	if(_name == "") _name = defaultDevelopmentModuleName();

	Log::log() << "Development Module is constructed with name: '" << _name << "' and will intialized from libpath: " << _moduleFolder.absoluteFilePath().toStdString() << std::endl;

	_developmentModuleName = _name;

	loadDescriptionFromFolder(_modulePackage);
	setInstalled(true);
}


void DynamicModule::initialize()
{

	//Log::log() << "DynamicModule::initialize() called for " << _moduleFolder.absolutePath() << std::endl;
	//Check some stuff
	_moduleFolder.makeAbsolute();

	if(!_moduleFolder.exists())				throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " does not exist!");
	else if(!_moduleFolder.isDir())			throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " is not a directory!");
//	else if(!_moduleFolder.isWritable())	throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " is not writable!");

	setInitialized(true);
	auto checkForExistence = [&](std::string name, bool isFile = false)
	{
		QString modPath = _moduleFolder.absolutePath() + "/" + nameQ() + "/" + QString::fromStdString(name);
		QFileInfo checkInfo(modPath);

		std::string errorMsg = "";

		if(!checkInfo.exists())	errorMsg = name + " is missing from " + modPath.toStdString();

		if(errorMsg == "" && !isFile	&& !checkInfo.isDir())	errorMsg = name + " is not, as expected, a directory";
		if(errorMsg == "" &&  isFile	&& !checkInfo.isFile())	errorMsg = name + " is not, as expected, a file";
		if(errorMsg != "")
		{
			throw std::runtime_error(errorMsg);
		}

		return checkInfo;
	};

	checkForExistence("icons");
	checkForExistence("qml");
	//checkForExistence("R"); The module is now a package so there is no point in checking for R code because what kind of R package has *no* r-code?

	//Ok everything seems to be in order, let's load!
	QString qmlTxt = "";
	
	QUrl url;
	try
	{
		QFileInfo descriptionInfo = checkForExistence(getQmlDescriptionFilename(), true);
		
		url = QUrl::fromLocalFile(descriptionInfo.absoluteFilePath());

		QFile descriptionFile(descriptionInfo.absoluteFilePath());
		descriptionFile.open(QFile::ReadOnly);

		qmlTxt = descriptionFile.readAll();

		if(qmlTxt == "")
			throw std::runtime_error(getQmlDescriptionFilename() + " is empty!");
	}
	catch(std::runtime_error & qmlNotFound)
	{
		throw std::runtime_error("Couldn't find " + getQmlDescriptionFilename());
	}
	
	loadDescriptionQml(qmlTxt, url);

	try
	{
		QFileInfo upgradesInfo = checkForExistence(getQmlUpgradesFilename(), true);
		
		url = QUrl::fromLocalFile(upgradesInfo.absoluteFilePath());

		QFile upgradesFile(upgradesInfo.absoluteFilePath());
	
		upgradesFile.open(QFile::ReadOnly);

		qmlTxt = upgradesFile.readAll();

		if(qmlTxt == "")
			throw std::runtime_error(getQmlUpgradesFilename() + " is empty!");
		
		loadUpgradesQML(qmlTxt, url);
	
	}
	catch(ModuleException & upgradeLoadError)
	{
		MessageForwarder::showWarning(tr("Loading upgrades for module %1 failed").arg(nameQ()), tr("Loading upgrades gave the following error(s): %1").arg(tq(upgradeLoadError.what())));
	}
	catch(qmlLoadError & loadError)
	{
		MessageForwarder::showWarning(tr("Loading upgrades for module %1 failed").arg(nameQ()), tr("Loading upgrades gave the following error(s): %1").arg(tq(loadError.what())));
	}
	catch(std::runtime_error & qmlNotFound)
	{
		; //Doesn't matter if this file couldn't be found or whatever
		Log::log() << "Loading " << getQmlUpgradesFilename() << " had the following std:runtime_error: '" << qmlNotFound.what() << "' this will be ignored." << std::endl;
	}
	
	QFile DESCRIPTION(checkForExistence("DESCRIPTION", true).absoluteFilePath());
	DESCRIPTION.open(QFile::ReadOnly);
	loadRequiredModulesFromDESCRIPTIONTxt(DESCRIPTION.readAll());
}

void DynamicModule::loadDescriptionFromFolder( const std::string & folderPath, bool onlyIfNotLoadedYet)
{
	if(onlyIfNotLoadedYet && _description)
		return;

	std::string descriptionQml  = getDescriptionQmlFromFolder(folderPath);

	if(descriptionQml == "")
		throw std::runtime_error("No description found in folder " + folderPath);
	
	QUrl url = QUrl(".");
	
	loadDescriptionQml(tq(descriptionQml), url);
	
	loadRequiredModulesFromFolder(_modulePackage);
}

void DynamicModule::loadDescriptionFromArchive(const std::string & archivePath)
{
	std::string descriptionQml  = getDescriptionQmlFromArchive(archivePath);

	if(descriptionQml == "")
		throw std::runtime_error("No description found in archive " + archivePath);

	QUrl url = QUrl(".");
	
	loadDescriptionQml(tq(descriptionQml), url);
	
	loadRequiredModulesFromArchive(archivePath);
}

///Very simple parse to go through list of imports and see if any modules are in it. Because we can't really know what is a module and what isn't... We just get em all.
void DynamicModule::loadRequiredModulesFromDESCRIPTIONTxt(const QString & DESCRIPTION)
{
	stringset reqModules;
	
	
	QRegularExpression isItImports("Imports:");
	

	QRegularExpressionMatch m = isItImports.match(DESCRIPTION);
	
	if(!m.hasMatch()) return;
	
	size_t importsEnd = m.capturedEnd();

	//Aka check for stuff like "name (...),", "name (...)", "name," or "name" 
	QRegularExpression pkgEx("[[:space:]]*([[:alnum:].]+)[[:space:]]*(\\([^)]\\))?(,)?");
	
	QRegularExpressionMatchIterator pkgMIt = pkgEx.globalMatch(DESCRIPTION, importsEnd);

	while(pkgMIt.hasNext())
	{
		QRegularExpressionMatch pkgM = pkgMIt.next();
		reqModules.insert(fq(pkgM.captured(1)));
		
		if(pkgM.captured(3) == "") //Not a comma so last one
			break;
	}		
	
	/*Log::log() << "loadRequiredModulesFromDESCRIPTIONTxt found: ";
	for(const std::string & reqM : reqModules)
		Log::log(false) << reqM << "\t";
	Log::log(false) << std::endl;*/
	
	setImportsR(reqModules);
}



Description * DynamicModule::instantiateDescriptionQml(const QString & descriptionTxt, const QUrl & url, const std::string & moduleName)
{
	Description * description = qobject_cast<Description*>(instantiateQml(descriptionTxt, url, moduleName, "Description", getQmlDescriptionFilename(), MainWindow::singleton()->giveRootQmlContext()));

	return description;
}


Upgrades * DynamicModule::instantiateUpgradesQml(const QString & upgradesTxt, const QUrl & url, const std::string & moduleName)
{
	Upgrades * upgrades = qobject_cast<Upgrades*>(instantiateQml(upgradesTxt, url, moduleName, "Upgrades", getQmlUpgradesFilename(), MainWindow::singleton()->giveRootQmlContext()));

	//Log::log() << "Dynamic module " << moduleName << " got upgrades? " << ( upgrades ? "yes!" : "no...") << std::endl;

	upgrades->setModule(tq(moduleName));

	return upgrades;
}


void DynamicModule::loadDescriptionQml(const QString & descriptionTxt, const QUrl & url)
{
	Description * description = instantiateDescriptionQml(descriptionTxt, url, name());

	if(!description)
		throw ModuleException(name(), getQmlDescriptionFilename() + " must have Description item as root!");

	description->setDynMod(this);

	loadInfoFromDescriptionItem(description);
}

void DynamicModule::loadUpgradesQML(const QString & upgradesTxt, const QUrl & url)
{
	Upgrades * upgrades = instantiateUpgradesQml(upgradesTxt, url, name());

	if(!upgrades)
		throw ModuleException(name(), getQmlUpgradesFilename() + " could not be instantiated!\nIt must have Upgrades item as root and, well not contain any errors.\nCheck the log for more details.");

	_upgrades = upgrades;
}

bool DynamicModule::hasUpgradesToApply(const std::string & function, const Version & version)
{
	return _upgrades != nullptr && _upgrades->hasUpgradesToApply(function, version);
}

void DynamicModule::applyUpgrade(const std::string	& function,	const Version & version, Json::Value & analysesJson, UpgradeMsgs & msgs, StepsTaken & stepsTaken)
{
	if(!analysesJson.isMember("dynamicModule"))
		analysesJson["dynamicModule"] = asJsonForJaspFile(function);
	
	_upgrades->applyUpgrade(function, version, analysesJson, msgs, stepsTaken);
}

void DynamicModule::loadInfoFromDescriptionItem(Description * description)
{
	if(_description && _description != description)
		delete _description;

	_description = description;

	if(_name != fq(description->name()))
		Log::log() << "Description has different name (" << description->name() << ") from DynMod (" << _name << ")" << std::endl;

	const std::string oldTitle		= _title;

	_title							= fq(description->title());
	_icon							= fq(description->icon());
	_author							= fq(description->author());
	_license						= fq(description->license());
	_website						= fq(description->website().toString());
	_maintainer						= fq(description->maintainer());
	_descriptionTxt					= fq(description->description());
	_version						= fq(description->version());
	_hasWrappers					= description->hasWrappers();

	for(auto * menuEntry : _menuEntries)
		delete menuEntry;
	_menuEntries.clear();

	_menuEntries = description->menuEntries();

	emit descriptionReloaded(this);

	if(oldTitle != _title)
		emit titleChanged();
	
	emit readyChanged(installed());
}



void DynamicModule::setReadyForUse()
{
	if(_status != moduleStatus::installNeeded)
		setStatus(moduleStatus::readyForUse);
}


Json::Value	DynamicModule::requestJsonForPackageInstallationRequest(bool onlyModPkg)
{
	if(!installNeeded())
	{
		Log::log() << "DynamicModule::requestJsonForPackageInstallationRequest(): Module (" << _name << ") thinks an install isn't needed, but requesting it anyway!" << std::endl;
	}

	Json::Value requestJson(Json::objectValue);

	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::installNeeded);
	requestJson["moduleName"]		= _name;
	requestJson["moduleCode"]		= generateModuleInstallingR(onlyModPkg);

	setInstalling(true);

	return requestJson;
}

Json::Value	DynamicModule::requestJsonForPackageLoadingRequest()
{
	Json::Value requestJson(Json::objectValue);

	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::loading);
	requestJson["moduleName"]		= _name;
	requestJson["moduleLibPaths"]   =  getLibPathsToUse();
	requestJson["moduleCode"]		= generateModuleLoadingR();

	return requestJson;
}

std::string DynamicModule::getLibPathsToUse() const
{
	return "c('" + shortenWinPaths(moduleRLibrary()).toStdString() + "', '" + AppDirs::rHome().toStdString() + "/library')";
}

///It would probably be better to move all of this code to jasp-r-pkg or something, but for now this works fine.
std::string DynamicModule::generateModuleInstallingR(bool onlyModPkg)
{
	std::stringstream R;

	if(_modulePackage == "")
	{
		Log::log() << "generateModuleInstallingR has some trouble because package was not unpacked anywhere..." << std::endl;

		setInstallLog("Installing module " + _name + " failed because package filepath was not specified");
		setStatus(moduleStatus::error);
		return "stop('No package specified!')";
	}

	try
	{
		loadDescriptionFromFolder(_modulePackage);
	}
	catch(ModuleException & e)
	{
		setInstallLog(e.what());
		setStatus(moduleStatus::error);
		return "stop('Something went wrong during intialization of the Description!\nMake sure it follows the standard set in https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-adding-module.md#descriptionqml\n')";
	}
	setInstallLog("Installing module " + _name + ".\n");
	return QString(
	R"readableR(
	tmp <- .libPaths();
	.libPaths("%1");
	Sys.setenv(MODULE_INSTALL_MODE="localizeModuleOnly");
	options("renv.config.install.verbose" = TRUE, "PKGDEPENDS_LIBRARY"="%2");
	result <- jaspModuleInstaller::installJaspModule(modulePkg='%3', moduleLibrary='%4', repos='%5', onlyModPkg=%6, force=TRUE, frameworkLibrary='%7');
	.libPaths(tmp);
	return(result);
	)readableR")
	.arg(AppDirs::bundledModulesDir() + "Tools/jaspModuleInstaller_library/")
	.arg(AppDirs::bundledModulesDir() + "Tools/pkgdepends_library/")
	.arg(tq(_modulePackage))
	.arg(moduleRLibrary())
	.arg(Settings::value(Settings::CRAN_REPO_URL).toString())
	.arg(onlyModPkg ? "TRUE" : "FALSE")
	.arg(AppDirs::rHome()+"/library")
	.toStdString();
}

std::string DynamicModule::generateModuleLoadingR(bool shouldReturnSucces)
{
	std::stringstream R;

	R << standardRIndent << "library('" << _name << "');\n";

	if(shouldReturnSucces)
		R << "return('"+succesResultString()+"')";


	return R.str();
}

std::string DynamicModule::generateModuleUnloadingR()
{
	std::stringstream out;

	out << _name << " <- NULL; gc(); return('"+succesResultString()+"')";

	//Log::log() << "DynamicModule(" << _name << ")::generateModuleUnloadingR() generated:\n" << out.str() << std::endl;

	return out.str();
}

std::string DynamicModule::generateModuleUninstallingR()
{
	QDir myLibrary(moduleRLibrary());

	if(!myLibrary.exists())
		return "";

	QStringList libraries = myLibrary.entryList(QDir::Filter::NoDotAndDotDot | QDir::Filter::Dirs);
	if(libraries.size() == 0)
		return "";

	std::stringstream out;

	for(const QString & library : libraries)
	{
		if(out.str().size() > 0)
			out << ", ";

		out << "'" << library.toStdString() << "'";
	}

	return "remove.packages(c(" + out.str() + ", lib='" + moduleRLibrary().toStdString() + "'); R.utils::gcDLLs(gc=TRUE, quiet=TRUE);";
}

std::string DynamicModule::moduleInstFolder() const
{
	//Because in a developer mod everything is loaded directly from the source folder we give an actual inst folder:
	if(_isDeveloperMod && !_isLibpathDevMod) return _modulePackage + "/inst";
	//But after install this is in a R-library and therefore there is no more inst folder:
	else				return moduleRLibrary().toStdString() + "/" + _name + "/";
}

std::string	DynamicModule::qmlFilePath(const std::string & qmlFileName)	const
{
	return qmlFolder() + qmlFileName;
}

std::string	DynamicModule::qmlFolder()	const
{
	return moduleInstFolder() + "/qml/";
}

std::string DynamicModule::iconFolder() const
{
	return moduleInstFolder() + "/icons/";
}

std::string DynamicModule::rModuleCall(const std::string &function) const
{
	return ".libPaths(" + getLibPathsToUse()  + ");\n"
			+ _name + "::" + function;
}

std::string	DynamicModule::iconFilePath(std::string whichIcon)	const
{
	if(!installed()) return "";

	return iconFolder() + (whichIcon == "" ? _icon : whichIcon);
}

QString DynamicModule::helpFolderPath() const
{
	return tq(moduleInstFolder() + "/help/");
}

///The helpcontents might contain relative paths, this is sadly enough not easy to solve because the HelpWindow relies on its url to find relevant things in Resources
///What we can do however is preprocess the markdown and replace certain string(s), in this case "%HELP_FOLDER%"
void DynamicModule::preprocessMarkdownHelp(QString & md) const
{
	md.replace("%HELP_FOLDER%",  helpFolderPath());
}


AnalysisEntry* DynamicModule::retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile) const
{
	std::string moduleName		= jsonFromJaspFile.get("moduleName", "Modulename wasn't actually filled!").asString(),
				moduleVersion	= jsonFromJaspFile.get("moduleVersion", "0.0.1337").asString();

	if(moduleName != name())
		throw ModuleException(name(), "Tried to load an AnalysisEntry for module (" + moduleName +") from me...");

	if(Version(moduleVersion) != version())
		std::cerr << "Loading analysis based on different version of module(" << moduleName << "), but going ahead anyway. Analysis based on version: " << moduleVersion << " and actual loaded version of module is: " << version() << std::endl;

	std::string codedReference = jsonFromJaspFile.get("analysisEntry", "AnalysisEntry's coded reference wasn't actually specified!").asString();

	return retrieveCorrespondingAnalysisEntry(codedReference);
}

AnalysisEntry* DynamicModule::retrieveCorrespondingAnalysisEntry(const std::string & codedReference) const
{
	auto parts = stringUtils::split(codedReference, '~');

	std::string moduleName		= parts.size() > 1 ? parts[0] : "",
				function		= parts.size() > 1 ? parts[1] : parts[0];

	if(!moduleName.empty() && _name != moduleName)
		throw Modules::ModuleException(_name, "This coded reference belongs to a different dynamic module, this one: "+moduleName);

	for (AnalysisEntry * menuEntry : _menuEntries)
		if (menuEntry->isAnalysis() && menuEntry->function() == function)
			return menuEntry;

	throw Modules::ModuleException(_name, "Cannot find analysis with function " + function + "...");
}

void DynamicModule::setInstallLog(std::string installLog)
{
	if (_installLog == installLog)
		return;

	_installLog = installLog;
	emit installLogChanged();
}

void DynamicModule::setInstallingSucces(bool succes)
{
	if(!succes)
		setStatus(moduleStatus::error);

	setInstallLog(installLog().toStdString() + "Installation " + (succes ? "succeeded" : "failed") + "\n");

	setInstalled(succes);
	setInstalling(false);
}


void DynamicModule::setInstalled(bool installed)
{
	if(_installed != installed)
	{
		_installed = installed;
		emit installedChanged(_installed);
	}

	if(installing())
		setInstalling(false);

	if(installed)
		setStatus(moduleStatus::readyForUse);
}

void DynamicModule::setInstalling(bool installing)
{
	if (_installing == installing)
		return;

	_installing = installing;
	emit installingChanged(_installing);
}

void DynamicModule::setInitialized(bool initialized)
{
	if (_initialized == initialized)
		return;

	_initialized = initialized;
	emit initializedChanged(_initialized);
}

void DynamicModule::setStatus(moduleStatus newStatus)
{
	if(_status != newStatus && (_status == moduleStatus::error || newStatus == moduleStatus::error))
		errorChanged(error());

	// if we already need an install then we should only install the modpkg
	if(_status == moduleStatus::installNeeded && newStatus == moduleStatus::installModPkgNeeded)
		return;

	bool readyForUseInvolved = ( newStatus == moduleStatus::readyForUse ) || ( _status == moduleStatus::readyForUse );

	_status = newStatus;

	switch(_status)
	{
	case moduleStatus::installNeeded:			emit registerForInstalling(_name);			setInstalled(false);					break;
	case moduleStatus::installModPkgNeeded:		emit registerForInstallingModPkg(_name);	setInstalled(false);					break;
	case moduleStatus::error:					Log::log() << "Just set an error on the status of module "<< _name << std::endl;	break;
	default:																				break;
	}

	emit statusChanged();

	if(readyForUseInvolved)
		emit readyForUseChanged();
}

void DynamicModule::reloadDescription()
{
	try
	{
		loadDescriptionFromFolder(fq(_moduleFolder.absoluteFilePath() + "/" + nameQ() + "/"), false);
	}
	catch(std::runtime_error e) { return; } //If it doesnt work then never mind.
}

std::string DynamicModule::getDESCRIPTIONFromArchive(const std::string &  filepath)
{
	try {			return ExtractArchive::extractSingleTextFileFromArchive(filepath, "DESCRIPTION"); }
	catch (...) {	return "";	}
}

std::string DynamicModule::getDESCRIPTIONFromFolder(const std::string & filepath)
{
	return getFileFromFolder(QString::fromStdString(filepath), "DESCRIPTION").toStdString();
}

std::string DynamicModule::getDescriptionQmlFromArchive(const std::string &  filepath)
{
	try {
		return ExtractArchive::extractSingleTextFileFromArchive(filepath, getQmlDescriptionFilename());
	} catch (...) {
		return "";
	}
}

std::string DynamicModule::getDescriptionQmlFromFolder(const std::string &  filepath)
{
	return getFileFromFolder(filepath, getQmlDescriptionFilename());
}

QString DynamicModule::getFileFromFolder(const QString &  filepath, const QString & searchMe)
{
	Log::log() << "Trying to find '" << searchMe.toStdString() << "' in folder: '" << filepath.toStdString() << "'" << std::endl;

	QDir dir(filepath);

	if(!dir.exists())
		return "";

	for(QString entry : dir.entryList(QDir::Filter::NoDotAndDotDot | QDir::Filter::Files))
		if(entry == searchMe)
		{
			Log::log() << "Found " << searchMe << "!" << std::endl;
			QFile foundIt(dir.absoluteFilePath(entry));
			foundIt.open(QFile::ReadOnly);
			return foundIt.readAll();
		}

	for(QString entry : dir.entryList(QDir::Filter::NoDotAndDotDot | QDir::Filter::Dirs))
	{
		QString foundThis = getFileFromFolder(dir.absoluteFilePath(entry), searchMe);
		if(foundThis != "")
			return foundThis;
	}

	return "";
}

std::string DynamicModule::getFileFromFolder(const std::string &  filepath, const std::string & searchMe)
{
	return getFileFromFolder(QString::fromStdString(filepath), QString::fromStdString(searchMe)).toStdString();
}


std::string DynamicModule::extractPackageNameFromDescriptionQmlTxt(const std::string & descriptionTxt)
{
	std::string foundName = "";

	if(descriptionTxt == "")
		return foundName;

	try
	{
		Description * desc = instantiateDescriptionQml(tq(descriptionTxt), QUrl::fromLocalFile(tq(getQmlDescriptionFilename())), "???");

		foundName = fq(desc->name());

		delete desc;
	}
	catch(Modules::ModuleException & e) { throw e; } //If qml cannot be parsed I want this error  in the users face
	catch(...){}

	return foundName;
}

std::string DynamicModule::extractPackageNameFromDescriptionJsonTxt(const std::string & descriptionTxt)
{
	std::string foundName = "";

	if(descriptionTxt != "")
	{
		Json::Value descriptionJson;
		Json::Reader().parse(descriptionTxt, descriptionJson);

		try
		{
			Json::Value & moduleDescription = descriptionJson["moduleDescription"];

			foundName = moduleDescription.get("name", "").asString();

			if(foundName == "")
				foundName = stringUtils::stripNonAlphaNum(moduleDescription.get("title", "").asString());
		}
		catch(...)
		{ }
	}

	return foundName;
}

std::string DynamicModule::extractPackageNameFromDESCRIPTIONTxt(const std::string & DESCRIPTION)
{
	const std::string PackageField = "Package:";

	auto PackagePos = DESCRIPTION.find_first_of(PackageField);

	std::string foundName = "";

	if(!(PackagePos == std::string::npos || PackagePos + PackageField.size() >= DESCRIPTION.size()))
	{
		//throw std::runtime_error("Trying to find package/module name in " + filepath + " but it cannot be found, make sure DESCRIPTION is available inside the archive and contains the field " + PackageField);

		PackagePos += PackageField.size();

		QString PackageName = QString::fromStdString(DESCRIPTION.substr(PackagePos, DESCRIPTION.find_first_of("\n", PackagePos) - PackagePos )).trimmed();

		if(PackageName != "")
			foundName = PackageName.toStdString();
	}

	return foundName;
}

std::string DynamicModule::extractPackageNameFromArchive(const std::string & archiveFilepath)
{
	Log::log() << "Trying to extract package name from archive '" << archiveFilepath << "'" << std::endl;

	std::string foundName = extractPackageNameFromDESCRIPTIONTxt(getDESCRIPTIONFromArchive(archiveFilepath));

	if(foundName == "") //Ok, so lets find it in QML ?
		foundName = extractPackageNameFromDescriptionQmlTxt(getDescriptionQmlFromArchive(archiveFilepath));

	if(foundName == "") //Then we will just use the archive name and see if that works..
	{
		const std::string	targz	= ".tar.gz",
							tgz		= ".tgz",
							zip		= ".zip";

		std::string fileName = archiveFilepath.find_last_of('/') == std::string::npos ? archiveFilepath : archiveFilepath.substr(archiveFilepath.find_last_of('/') + 1);

		auto removeExtension = [&](const std::string & ext){
			if(fileName.size() - ext.size() > 0 && fileName.substr(fileName.size() - ext.size()) == ext)
			{
				foundName = stringUtils::stripNonAlphaNum(fileName.substr(0, fileName.size() - ext.size()));
				return false;
			}
			return true;
		};

		removeExtension(targz) || removeExtension(tgz) || removeExtension(zip);
	}

	if(foundName == "") //what the hell?
		throw std::runtime_error("Trying to find package/module name in " + archiveFilepath + " but I can't...");

	Log::log() << "Found name: '" << foundName << "'" << std::endl;

	return foundName;
}

std::string DynamicModule::extractPackageNameFromFolder(const std::string & folderFilepath)
{
	Log::log() << "Trying to extract package name from folder '" << folderFilepath << "'" << std::endl;

	std::string foundName = extractPackageNameFromDescriptionQmlTxt(getDescriptionQmlFromFolder(folderFilepath));

	if(foundName == "") //Ok lets try to find DESCRIPTION then
		foundName = extractPackageNameFromDESCRIPTIONTxt(getDESCRIPTIONFromFolder(folderFilepath));

	if(foundName == "") //Then we will just use the folder name and see if that works..
	{
		QDir folder(QString::fromStdString(folderFilepath));

		foundName = folder.dirName().toStdString();
	}

	if(foundName == "") //what the hell?
		throw std::runtime_error("Trying to find package/module name in " + folderFilepath + " but I can't...");

	Log::log() << "Found name: '" << foundName << "'" << std::endl;

	return foundName;
}

Json::Value DynamicModule::asJsonForJaspFile(const std::string & analysisFunction) const
{
	Json::Value json(Json::objectValue);

	json["moduleName"]			= name();
	json["moduleVersion"]		= version();
	json["moduleMaintainer"]	= maintainer();
	json["moduleWebsite"]		= website();
	json["analysisEntry"]		= analysisFunction;

	return json;
}

bool DynamicModule::isDescriptionFile(const std::string & filename)
{
	return isDescriptionFile(tq(filename));
}

bool DynamicModule::isDescriptionFile(const QString & filename)
{
	//We force it all to lower to make sure we get the right result on mac or windows because their filesystems aren't case sensitive... But it would still be wrong I guess?
	return filename.toLower() == tq(getQmlDescriptionFilename()).toLower();
}

void DynamicModule::setBundled(bool bundled)
{
	if (_bundled == bundled)
		return;

	_bundled = bundled;
	emit bundledChanged(_bundled);
}

std::string DynamicModule::toString()
{
	std::stringstream out;
	out << "DynamicModule " << _name << "(0x" << std::hex << size_t(static_cast<void *>(this)) << ")" ;
	return out.str();
}

void DynamicModule::setImportsR(stringset importsR)
{
	if(importsR != _importsR)
	{
		_importsR = importsR;
		emit importsRChanged();
	}
}

stringset DynamicModule::requiredModules() const 
{
	stringset out;
	
	for(const std::string & pkg : _importsR)
		if(DynamicModules::dynMods()->dynamicModule(pkg))
			out.insert(pkg);
	
	return out;
}

QString DynamicModule::patchLibPathHelperFunc(QString libpath) {
#ifdef __APPLE__
	//we copy everything because we need to patch and resign it all
	auto path = std::filesystem::temp_directory_path() / Settings::value(Settings::DIRECT_DEVMOD_NAME).toString().toStdString();
	std::filesystem::remove_all(path);
	std::filesystem::copy(libpath.toStdString(), path, std::filesystem::copy_options::recursive);
	_moduleLibraryFixer(path, true, true, true);
	return tq(path.generic_string());
#else
	return libpath;
#endif

}


}




