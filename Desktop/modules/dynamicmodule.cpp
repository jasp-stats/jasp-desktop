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


#include "dynamicmodule.h"
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include <boost/filesystem.hpp>
#include "utilities/extractarchive.h"
#include "tempfiles.h"
#include <locale>
#include "log.h"
#include "utilities/qutils.h"
#include "utilities/languagemodel.h"
#include "dynamicmodules.h"
#include <QQmlContext>
#include <QQmlIncubator>
#include "description/description.h"
#include <QThread>

namespace Modules
{

ModuleException::ModuleException(std::string moduleName, std::string problemDescription)
	: std::runtime_error("Module \"" + moduleName + "\" had a problem: " + problemDescription)
{}

const char * standardRIndent = "  ";

std::string DynamicModule::_developmentModuleName = "?";

///This constructor takes the path to an installed jasp-module as path (aka a directory that contains a library of R packages, one of which is the actual module with QML etc)
DynamicModule::DynamicModule(QString moduleDirectory, QObject *parent, bool bundled, bool isCommon)
	: QObject(parent), _moduleFolder(moduleDirectory), _bundled(bundled), _isCommon(isCommon)
{
	QDir moduleDir(_moduleFolder.absoluteDir());
	if(!moduleDir.exists())
		throw std::runtime_error("Module folder '" + moduleDir.absolutePath().toStdString() + "' does not exist so cannot load from there!");

	_name		= stringUtils::stripNonAlphaNum(moduleDir.dirName().toStdString());

	setInstalled(true);
}

///This constructor takes the path to an R-package as first argument, this R-package must also be a jasp-module and will be installed to the app-directory for the particular OS it runs on.
DynamicModule::DynamicModule(std::string modulePackageFile, QObject *parent, bool unpack) : QObject(parent), _modulePackage(modulePackageFile)
{
	_name			= extractPackageNameFromArchive(modulePackageFile);
	_moduleFolder	= QFileInfo(AppDirs::userModulesDir() + QString::fromStdString(_name) + "/");
	unpackage();

	loadDescriptionFromFolder(_modulePackage);
}

QFileInfo DynamicModule::developmentModuleFolder()
{
	return QFileInfo(AppDirs::userModulesDir() + QString::fromStdString(defaultDevelopmentModuleName()) + "/");
}

void DynamicModule::developmentModuleFolderCreate()
{
	if(developmentModuleFolder().dir().exists()) return;

	QDir(AppDirs::userModulesDir()).mkdir(QString::fromStdString(defaultDevelopmentModuleName()));
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
}


void DynamicModule::initialize()
{
	//Check some stuff
	_moduleFolder.makeAbsolute();
	QDir moduleDir(_moduleFolder.absoluteDir());

	if(!_moduleFolder.exists())				throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " does not exist!");
	else if(!_moduleFolder.isDir())			throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " is not a directory!");
	else if(!_moduleFolder.isWritable())	throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " is not writable!");

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
	QString qmlTxt = "", jsonTxt = "";
	try
	{
		QFileInfo descriptionInfo = checkForExistence(getQmlDescriptionFilename(), true);

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

	loadDescriptionQml(qmlTxt);
}

void DynamicModule::loadDescriptionFromFolder( const std::string & folderPath)
{
	std::string descriptionQml  = getDescriptionQmlFromFolder(folderPath);

	if(descriptionQml == "")
		throw std::runtime_error("No description found in folder " + folderPath);

	loadDescriptionQml(tq(descriptionQml));
}

void DynamicModule::loadDescriptionFromArchive(const std::string & archivePath)
{
	std::string descriptionQml  = getDescriptionQmlFromArchive(archivePath);

	if(descriptionQml == "")
		throw std::runtime_error("No description found in archive " + archivePath);

	loadDescriptionQml(tq(descriptionQml));
}

//Turning QMLENGINE_DOES_ALL_THE_WORK on also works fine, but has slightly less transparent errormsgs so isn't recommended
//#define QMLENGINE_DOES_ALL_THE_WORK

Description * DynamicModule::instantiateDescriptionQml(QString descriptionTxt, QUrl url, const std::string & moduleName)
{	
	QObject * obj = nullptr;
#ifdef QMLENGINE_DOES_ALL_THE_WORK
	obj = DynamicModules::dynMods()->loadQmlData(descriptionTxt, url);
#else
	QQmlContext * ctxt = DynamicModules::dynMods()->requestRootContext();

	QQmlComponent descriptionQmlComp(ctxt->engine());

	//Log::log() << "Setting url to '" << url.toString() << "' for Description.qml.\n" << std::endl;// data: '" << descriptionTxt << "'\n"<< std::endl;

	descriptionQmlComp.setData(descriptionTxt.toUtf8(), url);

	if(descriptionQmlComp.isLoading())
		Log::log() << "Description for module " << moduleName << " is still loading, make sure you load a local file and that Windows doesnt mess this up for you..." << std::endl;


	auto errorLogger =[&](bool isError, QList<QQmlError> errors)
	{
		if(!isError) return;

		std::stringstream out;

		out << "Loading "+ getQmlDescriptionFilename() + " for module " + moduleName << " had errors:\n";

		for(const QQmlError error : errors)
			out << error.toString() << "\n";

		Log::log() << out.str() << std::flush;

		throw ModuleException(moduleName, "There were errors loading " + getQmlDescriptionFilename() + ":\n" + out.str());
	};

	errorLogger(descriptionQmlComp.isError(), descriptionQmlComp.errors());

	if(!descriptionQmlComp.isReady())
		throw ModuleException(moduleName, "Description Component is not ready!");

	QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
	descriptionQmlComp.create(localIncubator, ctxt);

	errorLogger(localIncubator.isError(), localIncubator.errors());

	obj = localIncubator.object();

#endif

	Description * description = qobject_cast<Description*>(obj);

	return description;
}

void DynamicModule::loadDescriptionQml(QString descriptionTxt)
{
	Description * description = instantiateDescriptionQml(descriptionTxt, QUrl("."), name());

	if(!description)
		throw ModuleException(name(), getQmlDescriptionFilename() + " must have Description object as root!");

	description->setDynMod(this);

	loadInfoFromDescriptionItem(description);
}

void DynamicModule::loadInfoFromDescriptionItem(Description * description)
{
	if(_descriptionObj && _descriptionObj != description)
		delete _descriptionObj;

	_descriptionObj = description;

	if(_name != fq(description->name()))
		Log::log() << "Description has different name (" << description->name() << ") from DynMod (" << _name << ")" << std::endl;

	_title							= fq(description->title());
	_icon							= fq(description->icon());
	_author							= fq(description->author());
	_license						= fq(description->license());
	_website						= fq(description->website().toString());
	_maintainer						= fq(description->maintainer());
	_description					= fq(description->description());
	_version						= fq(description->version());

	setRequiredPackages(description->requiredPackages());
	setRequiredModules(description->requiredModules());

	for(auto * menuEntry : _menuEntries)
		delete menuEntry;
	_menuEntries.clear();

	_menuEntries = description->menuEntries();

	emit descriptionReloaded(this);
}

void DynamicModule::setLoadingNeeded()
{
	if(_status != moduleStatus::installNeeded)
		setStatus(moduleStatus::loadingNeeded);
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

	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::loadingNeeded);
	requestJson["moduleName"]		= _name;
	requestJson["moduleCode"]		= generateModuleLoadingR();

	setLoading(true);

	return requestJson;
}

Json::Value	DynamicModule::requestJsonForPackageUnloadingRequest()
{
	Json::Value requestJson(Json::objectValue);

	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::unloadingNeeded);
	requestJson["moduleName"]		= _name;
	requestJson["moduleCode"]		= generateModuleUnloadingR();

	return requestJson;
}

void DynamicModule::unpackage()
{
	if(_modulePackage == "")
	{
		Log::log() << "DynamicModule::unpackage has some trouble because _modulePackage is empty..." << std::endl;

		setInstallLog("Installing module " + _name + " failed because package filepath was not specified");
		setStatus(moduleStatus::error);
		return;
	}

	std::string tmpDir = TempFiles::createTmpFolder(),
				modDir = tmpDir + "/" + _name;

	ExtractArchive::extractArchiveToFolder(_modulePackage, modDir);

	Log::log() << "Unpacked module to folder " << modDir << std::endl;

	_modulePackage = modDir;

}

std::string DynamicModule::getLibPathsToUse()
{
	std::string libPathsToUse = "c('" + moduleRLibrary().toStdString()	+ "'";

	std::vector<std::string> requiredLibPaths = fq(requiredModulesLibPaths(tq(_name)));

	for(const std::string & path : requiredLibPaths)
		libPathsToUse += ", '" + path + "'";

	libPathsToUse += ", .libPaths())"; //Because in stable it was ", .libPaths()"

	return libPathsToUse;
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

	std::string typeInstall = "'source'";

	//<< ".runSeparateR(\"{"

	std::string libPathsToUse = getLibPathsToUse();

	R << standardRIndent << "loadLog <- '';\n";
	
#if  defined(_WIN32) || defined(__APPLE__)
	R << "options(install.packages.compile.from.source = 'never')\n";
#endif

	auto installDeps = [&](const std::string & pkg)
	{
		R	<< standardRIndent << "withr::with_libpaths(new=" << libPathsToUse << ", remotes::install_deps(pkg= "	<< pkg << ", lib='" << moduleRLibrary().toStdString() << "',  INSTALL_opts=c('--no-test-load --no-multiarch'), upgrade='never', repos='" << Settings::value(Settings::CRAN_REPO_URL).toString().toStdString() << "'));\n";
	};

	auto installLocal = [&](std::string pkgPath)
	{
		R	<< standardRIndent << "pkgPath <- sub('\\\\', '/', " << pkgPath << ", fixed=TRUE);\n"; //replace any backslashes by forward slashes...
		R	<< standardRIndent << "print(paste0(\"pkgPath: '\", pkgPath, \"'\"));\n";
		R	<< standardRIndent << "loadLog <- paste0(loadLog, '\\n', .runSeparateR(paste0(\"withr::with_libpaths(new=" << libPathsToUse << ", pkgbuild::with_build_tools(install.packages(pkgs='\", pkgPath, \"', lib='" << moduleRLibrary().toStdString() << "', type='source', repos=NULL, INSTALL_opts=c('--no-multiarch')), required=FALSE ))\")));\n";
	};

	if(!onlyModPkg)
	{
		//Install dependencies:
		//First the ones from CRAN because they cant depend on one from github
		installDeps("'" + _modulePackage + "/.'");

		//Then the specials from github
		for(const Json::Value & reqPkg : _requiredPackages)
			if(reqPkg.isMember("github"))
			{
				R	 << standardRIndent << "print('Downloading pkg \"" << reqPkg["github"].asString() << "\" from github!');\n"
					 << standardRIndent << "pkgdownload <- remotes::remote_download(remotes::github_remote(repo= '"	<< reqPkg["github"].asString() << "'" << (reqPkg.isMember("gitref") ? ", ref='" + reqPkg["gitref"].asString() + "'" : "") << "));\n";
				installDeps("pkgdownload");
				installLocal("pkgdownload");
			}

		//And fix Mac OS libraries of dependencies:
		R << standardRIndent << ".postProcessLibraryModule(\"" << moduleRLibrary().toStdString() << "\");\n";
	}

		//Remove old copy of library (because we might be reinstalling and want the find.package check on the end to fail if something went wrong)
	R	<< standardRIndent << "tryCatch(expr={"				"withr::with_libpaths(new=" << libPathsToUse << ", { find.package(package='" << _name << "'); remove.packages(pkg='"	<< _name << "', lib='" << moduleRLibrary().toStdString() << "');})}, error=function(e) {});\n";

	R	<< standardRIndent << "print('Module library now looks like: ');\n" << standardRIndent << "print(list.files(path='" << moduleRLibrary().toStdString() << "', recursive=FALSE));\n";

		//Install module
	installLocal("'" + _modulePackage + "/.'");


		//Check if install worked and through loadlog as error otherwise
	std::string moduleNotFoundMsg = "'\\nCouldn\\'t find the module by name of " + _name + "'";
	
	R << standardRIndent << "tryCatch(expr={ withr::with_libpaths(new='" << moduleRLibrary().toStdString() << "', find.package(package='" << _name << "')); return('" << succesResultString() << "');}, error=function(e) { .setLog(paste0(loadLog, " << moduleNotFoundMsg <<")); return('fail'); });\n";


	//Log::log() << "DynamicModule(" << _name << ")::generateModuleInstallingR() generated:\n" << R.str() << std::endl;

	return R.str();
}

std::string DynamicModule::generateModuleLoadingR(bool shouldReturnSucces)
{
	std::stringstream R;

	setLoadLog("Module " + _name + " is loading from " + _moduleFolder.absolutePath().toStdString() + "\n");

	//Add the module name to the "do not remove from global env" list in R. See jaspRCPP_purgeGlobalEnvironment
	R << "jaspBase:::.addModuleToDoNotRemove('" << _name << _modulePostFix << "');\n";

	R << _name << _modulePostFix << " <- module({\n" << standardRIndent << ".libPaths(" << getLibPathsToUse() <<");\n\n";

	for(const std::string & reqMod : _requiredModules)
		R << standardRIndent << "import('" << reqMod << "');\n";
	R << standardRIndent << "import('" << _name << "');\n\n";

	size_t maxL = 0;
	for(const AnalysisEntry * analysis : _menuEntries)
		if(analysis->shouldBeExposed())
			maxL = std::max(analysis->function().size(), maxL);

	auto filler = [](size_t spaces)
	{
		std::stringstream out;
		for(size_t i=0; i<spaces; i++)
			out << " ";
		return out.str();
	};

	for(const AnalysisEntry * analysis : _menuEntries)
		if(analysis->shouldBeExposed())
			R << standardRIndent << analysis->function() << _exposedPostFix << filler(maxL - analysis->function().size()) << " <- function(...) " << analysis->function() << "(...)\n";
	R << "})\n";

	if(shouldReturnSucces)
		R << "return('"+succesResultString()+"')";

	//Log::log() << "DynamicModule(" << _name << ")::generateModuleLoadingR() generated:\n" << R.str() << std::endl;

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
	if(_isDeveloperMod)	return _modulePackage + "/inst";
	//But after install this is in a R-library and therefore there is no more inst folder:
	else				return moduleRLibrary().toStdString() + "/" + _name + "/";
}

std::string	DynamicModule::qmlFilePath(const std::string & qmlFileName)	const
{
	return moduleInstFolder() + "/qml/" + qmlFileName;
}

std::string DynamicModule::iconFolder() const
{
	return moduleInstFolder() + "/icons/";
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

AnalysisEntry* DynamicModule::retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile) const
{
	std::string moduleName		= jsonFromJaspFile.get("moduleName", "Modulename wasn't actually filled!").asString(),
				moduleVersion	= jsonFromJaspFile.get("moduleVersion", "0.0.1337").asString();

	if(moduleName != name())
		throw ModuleException(name(), "Tried to load an AnalysisEntry for module (" + moduleName +") from me...");

	if(moduleVersion != version())
		std::cerr << "Loading analysis based on different version of module(" << moduleName << "), but going ahead anyway. Analysis based on version: " << moduleVersion << " and actual loaded version of module is: " << version() << std::endl;

	std::string codedReference = jsonFromJaspFile.get("analysisEntry", "AnalysisEntry's coded reference wasn't actually specified!").asString();

	return retrieveCorrespondingAnalysisEntry(codedReference);
}

AnalysisEntry* DynamicModule::retrieveCorrespondingAnalysisEntry(const std::string & codedReference) const
{
	auto parts = stringUtils::splitString(codedReference, '~');

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

void DynamicModule::setLoadLog(std::string loadLog)
{
	if (_loadLog == loadLog)
		return;

	_loadLog = loadLog;
	emit loadLogChanged();
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
}

void DynamicModule::setLoadingSucces(bool succes)
{
	setStatus(succes ? moduleStatus::readyForUse		: moduleStatus::error);
	setLoadLog(loadLog().toStdString() + "Loading " + (succes ? "succeeded" : "failed") + "\n");

	setLoaded(succes);
	setLoading(false);
}

void DynamicModule::setUnloaded()
{
	setLoaded(false);
	setLoading(false);
}

void DynamicModule::setLoaded(bool loaded)
{
	if(_loaded != loaded)
	{
		_loaded = loaded;

		emit loadedChanged(_loaded);
	}
}

void DynamicModule::setLoading(bool loading)
{
	if (_loading == loading)
		return;

	_loading = loading;
	emit loadingChanged(_loading);
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
	if(_status == newStatus)
		return;

	// if we already need an install then we should only install the modpkg
	if(_status == moduleStatus::installNeeded && newStatus == moduleStatus::installModPkgNeeded)
		return;

	_status = newStatus;

	emit statusChanged();

	switch(_status)
	{
	case moduleStatus::loadingNeeded:			emit registerForLoading(_name);				break;
	case moduleStatus::installNeeded:			emit registerForInstalling(_name);			break;
	case moduleStatus::installModPkgNeeded:		emit registerForInstallingModPkg(_name);	break;
	case moduleStatus::error:
		Log::log() << "Just set an error on the status of module "<< _name << std::endl;	break;
	default:																				break;
	}
}

void  DynamicModule::setRequiredPackages(Json::Value requiredPackages)
{
	if (_requiredPackages == requiredPackages)
		return;

	_previousReqPkgs  = _requiredPackages;
	_requiredPackages = requiredPackages;
	emit requiredPackagesChanged();
}

void DynamicModule::reloadDescription()
{
	try
	{
		loadDescriptionFromFolder(fq(_moduleFolder.absoluteFilePath() + "/" + nameQ() + "/"));
	}
	catch(std::runtime_error e) { return; } //If it doesnt work then never mind.

	if(_requiredPackages.toStyledString() != _previousReqPkgs.toStyledString())
	{
		Log::log() << "Required packages of module '" << _name << "' changed, installing again" << std::endl;
		setStatus(moduleStatus::installNeeded);
	}
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

bool DynamicModule::requiresData() const
{
	for(const AnalysisEntry * entry : _menuEntries)
		if(!entry->requiresData())
			return false;

	return true;
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
	out << "DynamicModule " << _name << "(0x" << std::hex << std::to_string(size_t(static_cast<void *>(this))) << ")" ;
	return out.str();
}

QStringList DynamicModule::requiredModulesQ() const {	return tql(_requiredModules);					}

void DynamicModule::setRequiredModules(stringset requiredModules)
{
	bool ditto = requiredModules.size() == _requiredModules.size();

	if(ditto)
		for(const std::string & mod : requiredModules)
			if(_requiredModules.count(mod) == 0)
			{
				ditto = false;
				break;
			}

	if(!ditto)
	{
		Log::log() << "Required Modules for module '" << name() << "' changed!" << std::endl;

		_requiredModules = requiredModules;

		emit requiredModulesChanged();
	}
}

}
