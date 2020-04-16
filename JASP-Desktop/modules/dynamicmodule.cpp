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

namespace Modules
{

ModuleException::ModuleException(std::string moduleName, std::string problemDescription)
	: std::runtime_error("Module \"" + moduleName + "\" had a problem: " + problemDescription)
{}

const char * standardRIndent = "  ";

std::string DynamicModule::_developmentModuleName = "?";

///This constructor takes the path to an installed jasp-module as path (aka a directory that contains a library of R packages, one of which is the actual module with QML etc)
DynamicModule::DynamicModule(QString moduleDirectory, QObject *parent) : QObject(parent), _moduleFolder(moduleDirectory)
{
	QDir moduleDir(_moduleFolder.absoluteDir());
	_name = stringUtils::stripNonAlphaNum(moduleDir.dirName().toStdString());
	setInstalled(true);
}

///This constructor takes the path to an R-package as first argument, this R-package must also be a jasp-module and will be installed to the app-directory for the particular OS it runs on.
DynamicModule::DynamicModule(std::string modulePackageFile, QObject *parent) : QObject(parent), _modulePackage(modulePackageFile)
{
	_name			= extractPackageNameFromArchive(modulePackageFile);
	_moduleFolder	= QFileInfo(AppDirs::modulesDir() + QString::fromStdString(_name) + "/");
	unpackage();

	parseDescriptionFile(getDescriptionJsonFromFolder(_modulePackage));
}

QFileInfo DynamicModule::developmentModuleFolder()
{
	return QFileInfo(AppDirs::modulesDir() + QString::fromStdString(defaultDevelopmentModuleName()) + "/");
}


void DynamicModule::developmentModuleFolderCreate()
{
	if(developmentModuleFolder().dir().exists()) return;

	QDir(AppDirs::modulesDir()).mkdir(QString::fromStdString(defaultDevelopmentModuleName()));
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

	QFileInfo descriptionInfo = checkForExistence(getJsonDescriptionFilename().toStdString(), true);
								checkForExistence("icons");
								checkForExistence("qml");
								//checkForExistence("R"); The module is now a package so there is no point in checking for R code because what kind of R package has *no* r-code?

	//Ok everything seems to be in order, let's load!

	QFile descriptionFile(descriptionInfo.absoluteFilePath());
	descriptionFile.open(QFile::ReadOnly);
	std::string	descriptionTxt(descriptionFile.readAll().toStdString());

	parseDescriptionFile(descriptionTxt);
}

void DynamicModule::parseDescriptionFile(std::string descriptionTxt)
{

	Json::Value descriptionJson;
	Json::Reader().parse(descriptionTxt, descriptionJson);

	try
	{
		Json::Value & moduleDescription = descriptionJson["moduleDescription"];
		_title							= moduleDescription.get("title",			_name).asString();
		_icon							= moduleDescription.get("icon",				"").asString();
		_author							= moduleDescription.get("author",			"Unknown").asString();
		_license						= moduleDescription.get("license",			"Unknown").asString();
		_website						= moduleDescription.get("website",			"Unknown").asString();
		_maintainer						= moduleDescription.get("maintainer",		"JASP Team <info@jasp-stats.org>").asString();
		_description					= moduleDescription.get("description",		"The R Code belonging to module " + _name).asString();
		auto jsonVer					= moduleDescription.get("version",			"0.0.0");
		_version						= jsonVer.isString() ? jsonVer.asString() : "0.0.0";

		setRequiredPackages(descriptionJson.get("requiredPackages", Json::arrayValue));

		for(auto * menuEntry : _menuEntries)
			delete menuEntry;
		_menuEntries.clear();

		bool defaultRequiresData = moduleDescription.get("requiresData", true).asBool();

		for(Json::Value & menuEntry : descriptionJson["menu"])
			_menuEntries.push_back(new AnalysisEntry(menuEntry, this, defaultRequiresData));

		emit descriptionReloaded(this);
	}
	catch(std::exception e)
	{
		throw std::runtime_error("During the parsing of the description.json of the Module " + _name + " something went wrong: " + e.what());
	}
}

void DynamicModule::setLoadingNeeded()
{
	if(_status != moduleStatus::installNeeded)
		setStatus(moduleStatus::loadingNeeded);
}

void DynamicModule::generateRPackageMetadata(QDir packageDir)
{
	QFile	descriptionFile(packageDir.absoluteFilePath("DESCRIPTION")),
			namespaceFile(	packageDir.absoluteFilePath("NAMESPACE"));

	if(!descriptionFile.exists() || Settings::value(Settings::DEVELOPER_MODE_REGENERATE_DESCRIPTION_ETC).toBool())
	{
		descriptionFile.open(QFile::WriteOnly	| QFile::Truncate);
		descriptionFile.write(generateDescriptionFileForRPackage().c_str());
	}

	if(!namespaceFile.exists() || Settings::value(Settings::DEVELOPER_MODE_REGENERATE_DESCRIPTION_ETC).toBool())
	{
		namespaceFile.open(QFile::WriteOnly		| QFile::Truncate);
		namespaceFile.write(generateNamespaceFileForRPackage().c_str());
	}
}

std::string DynamicModule::generateDescriptionFileForRPackage()
{
	std::stringstream out;

	out << "Package: "		<< _name <<
		"\nType: Package"
		"\nTitle: "			<< _title << " Module for JASP"
		"\nVersion: "		<< _version <<
		"\nDate: "			<< QDateTime::currentDateTime().toString("yyyy-MM-dd").toStdString() <<
		"\nAuthor: "		<< _author <<
		"\nWebsite: "		<< _website <<
		"\nMaintainer: "	<< _maintainer <<
		"\nDescription: "	<< _description <<
		"\nLicense: "		<< _license;


	if(_requiredPackages.isArray() && _requiredPackages.size() > 0)
	{
		out << "\nImports: ";

		bool first = true;
		for(Json::Value & pkgV : _requiredPackages)
		{
			if(!first) out << ", ";

			if(pkgV.isObject() && pkgV.isMember("package"))
			{
				try {
					out << pkgV["package"].asString();

					if(!pkgV["version"].isNull())
						out << " (>= " << pkgV["version"].asString() << ")";

					first = false;
				} catch (...) {
					setInstallLog("Something went wrong parsing the required packages in description.json! Make sure they follow the standard set in https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-adding-module.md#descriptionjson");
					setStatus(moduleStatus::error);
				}
			}
			else if(pkgV.isString())
			{
				out << pkgV.asString();
				first = false;
			}
		}
	}

	return out.str();

}

std::string DynamicModule::generateNamespaceFileForRPackage()
{
	std::stringstream out;

	for(const AnalysisEntry * analysis : _menuEntries)
		if(analysis->isAnalysis())
			out << "export(" << analysis->function() << ")\n";

	try
	{
		if(_requiredPackages.isArray())
			for(Json::Value & pkgV : _requiredPackages)
				out << standardRIndent << "import('" << (pkgV.isString()  ? pkgV.asString() : pkgV["package"].asString()) << "');\n";
	} catch (...) {
		throw std::runtime_error("Something went wrong parsing the required packages in description.json! Make sure they follow the standard set in https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-adding-module.md#descriptionjson");
	}

	return out.str();
}

Json::Value	DynamicModule::requestJsonForPackageInstallationRequest(bool onlyModPkg)
{
	if(!installNeeded())
	{
		Log::log() << "DynamicModule::requestJsonForPackageInstallationRequest(): Module (" << _name << ") is already installed!" << std::endl;
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

std::string DynamicModule::generateModuleInstallingR(bool onlyModPkg)
{
	std::stringstream R;


	if(_modulePackage == "")
	{
		Log::log() << "generateModuleInstallingR has some trouble because package was not unpacked anywehere..." << std::endl;

		setInstallLog("Installing module " + _name + " failed because package filepath was not specified");
		setStatus(moduleStatus::error);
		return "stop('No package specified!')";
	}

	parseDescriptionFile(getDescriptionJsonFromFolder(_modulePackage));

	try
	{
		generateRPackageMetadata(QDir(_modulePackage.c_str())); //Generate DESCRIPTION and NAMESPACE in case they are missing
	}
	catch(std::runtime_error & e)
	{
		setInstallLog(e.what());
		setStatus(moduleStatus::error);
		return "stop('Something went wrong parsing the required packages in description.json! Make sure they follow the standard set in https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-adding-module.md#descriptionjson')";
	}
	setInstallLog("Installing module " + _name + ".\n");

	std::string typeInstall = "'source'";
//<< ".runSeparateR(\"{"

	std::string libPathsToUse = "c('" + moduleRLibrary().toStdString()	+ "', .libPaths(.Library))";

	const char * pkgType =
#ifdef __linux__
							"source";
#else
							"binary";
#endif

	if(!onlyModPkg)	//First install dependencies:
		R	<< standardRIndent <<								"withr::with_libpaths(new=" << libPathsToUse << ", remotes::install_deps(pkg= '"	<< _modulePackage << "',   lib='" << moduleRLibrary().toStdString() << "', type='" << pkgType << "',  INSTALL_opts=c('--no-test-load --no-multiarch'), upgrade=FALSE, repos='" << Settings::value(Settings::CRAN_REPO_URL).toString().toStdString() << "'));\n"
		//And fix Mac OS libraries of dependencies:
		<< standardRIndent << "postProcessModuleInstall(\"" << moduleRLibrary().toStdString() << "\");\n";

		//Remove old copy of library (because we might be reinstalling and want the find.package check on the end to fail if something went wrong)
	R	<< standardRIndent << "tryCatch(expr={"				"withr::with_libpaths(new=" << libPathsToUse << ", { find.package(package='" << _name << "'); remove.packages(pkg='"	<< _name << "', lib='" << moduleRLibrary().toStdString() << "');})}, error=function(e) {});\n"

		//Install module
		<< standardRIndent << "loadLog <- .runSeparateR(\""	"withr::with_libpaths(new=" << libPathsToUse << ", install.packages(pkgs='"			<< _modulePackage << "/.', lib='" << moduleRLibrary().toStdString() << "', type=" << typeInstall << ", repos=NULL, INSTALL_opts=c('--no-multiarch')))\");\n" //Running in separate R because otherwise we cannot capture output :s

		//Check if install worked and through loadlog as error otherwise
		<< standardRIndent << "tryCatch(expr={"				"withr::with_libpaths(new=" << libPathsToUse << ", find.package(package='" << _name << "')); return('" << succesResultString() << "');}, error=function(e) { .setLog(loadLog); return('fail'); });\n";


	Log::log() << "DynamicModule(" << _name << ")::generateModuleInstallingR() generated:\n" << R.str() << std::endl;

	return R.str();
}

std::string DynamicModule::generateModuleLoadingR(bool shouldReturnSucces)
{
	std::stringstream R;

	setLoadLog("Module " + _name + " is being loaded from " + _moduleFolder.absolutePath().toStdString() + "\n");

	R << _name << " <- module({\n" << standardRIndent << ".libPaths('" << moduleRLibrary().toStdString() << "');\n";
	R << standardRIndent << "import('" << _name << "');\n\n";

	size_t maxL = 0;
	for(const AnalysisEntry * analysis : _menuEntries)
		if(analysis->isAnalysis())
			maxL = std::max(analysis->function().size(), maxL);

	auto filler = [](size_t spaces)
	{
		std::stringstream out;
		for(int i=0; i<spaces; i++)
			out << " ";
		return out.str();
	};

	for(const AnalysisEntry * analysis : _menuEntries)
		if(analysis->isAnalysis())
			R << standardRIndent << analysis->function() << _exposedPostFix << filler(maxL - analysis->function().size()) << " <- function(...) " << analysis->function() << "(...)\n";
	R << "})\n";

	if(shouldReturnSucces)
		R << "return('"+succesResultString()+"')";

	Log::log() << "DynamicModule(" << _name << ")::generateModuleLoadingR() generated:\n" << R.str() << std::endl;

	return R.str();
}

std::string DynamicModule::generateModuleUnloadingR()
{
	std::stringstream out;

	out << _name << " <- NULL; gc(); return('"+succesResultString()+"')";

	Log::log() << "DynamicModule(" << _name << ")::generateModuleUnloadingR() generated:\n" << out.str() << std::endl;

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

std::string	DynamicModule::qmlFilePath(const std::string & qmlFileName)	const
{
	if(_isDeveloperMod)	return _modulePackage + "/inst/qml/" + qmlFileName;
	else				return moduleRLibrary().toStdString() + "/" + _name + "/qml/" + qmlFileName;
}

std::string DynamicModule::iconFolder() const
{
	if(_isDeveloperMod)	return _modulePackage + "/inst/icons/";
	else				return moduleRLibrary().toStdString() + "/" + _name + "/icons/";
}

std::string	DynamicModule::iconFilePath(std::string whichIcon)	const
{
	return iconFolder() + (whichIcon == "" ? _icon : whichIcon);
}

QString DynamicModule::helpFolderPath() const
{
	if(_isDeveloperMod)	return QString::fromStdString(_modulePackage + "/inst/help/");
	else				return moduleRLibrary() + "/" + QString::fromStdString(_name) + "/help/";
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
	QFile descriptionFile(_moduleFolder.absoluteFilePath() + "/" + nameQ() + "/" + getJsonDescriptionFilename());

	descriptionFile.open(QIODevice::ReadOnly);

	Json::Value descriptionJson;
	std::string descriptionFileText = descriptionFile.readAll().toStdString();
	if(descriptionFileText.size() == 0) //ignore it when it is empty
		return;

	parseDescriptionFile(descriptionFileText);

	if(_requiredPackages != _previousReqPkgs)
	{
		Log::log() << "Required packages of module '" << _name << "' changed, installing again" << std::endl;
		setStatus(moduleStatus::installNeeded);
	}
}

std::string DynamicModule::getDESCRIPTIONFromArchive(const std::string &  filepath)
{
	try {
		return ExtractArchive::extractSingleTextFileFromArchive(filepath, "DESCRIPTION");
	} catch (...) {
		return "";
	}
}

std::string DynamicModule::getDESCRIPTIONFromFolder(const std::string & filepath)
{
	return getFileFromFolder(QString::fromStdString(filepath), "DESCRIPTION").toStdString();
}

std::string DynamicModule::getDescriptionJsonFromArchive(const std::string &  filepath)
{
	try {
		return ExtractArchive::extractSingleTextFileFromArchive(filepath, getJsonDescriptionFilename().toStdString());
	} catch (...) {
		return "";
	}
}

std::string DynamicModule::getDescriptionJsonFromFolder(const std::string &  filepath)
{
	return getFileFromFolder(QString::fromStdString(filepath), getJsonDescriptionFilename()).toStdString();
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

	if(foundName == "") //Ok lets try to find description.json then
		foundName = extractPackageNameFromDescriptionJsonTxt(getDescriptionJsonFromArchive(archiveFilepath));

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

	std::string foundName = extractPackageNameFromDESCRIPTIONTxt(getDESCRIPTIONFromFolder(folderFilepath));

	if(foundName == "") //Ok lets try to find description.json then
		foundName = extractPackageNameFromDescriptionJsonTxt(getDescriptionJsonFromFolder(folderFilepath));

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

}
