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

namespace Modules
{

ModuleException::ModuleException(std::string moduleName, std::string problemDescription)
	: std::runtime_error("Module \"" + moduleName + "\" had a problem: " + problemDescription)
{}

const char * standardRIndent = "  ";

///This constructor takes the path to an installed jasp-module as path (aka a directory that contains a library of R packages, one of which is the actual module with QML etc)
DynamicModule::DynamicModule(QString moduleDirectory, QObject *parent) : QObject(parent), _moduleFolder(moduleDirectory)
{
	QDir moduleDir(_moduleFolder.absoluteDir());
	_name = moduleNameStripNonAlphaNum(moduleDir.dirName().toStdString());
	setInstalled(true);
}

///This constructor takes the path to an R-package as first argument, this R-package must also be a jasp-module and will be installed to the app-directory for the particular OS it runs on.
DynamicModule::DynamicModule(std::string modulePackageFile, QObject *parent) : QObject(parent), _modulePackage(modulePackageFile)
{
	_name			= extractPackageNameFromArchive(modulePackageFile);
	_moduleFolder	= QFileInfo(AppDirs::modulesDir() + QString::fromStdString(_name) + "/");
	unpackage();
}

///This constructor is meant specifically for the development module and only it!
DynamicModule::DynamicModule(QObject * parent) : QObject(parent), _isDeveloperMod(true)
{
	_name			= developmentModuleName();
	_moduleFolder	= QFileInfo(AppDirs::modulesDir() + QString::fromStdString(_name) + "/");
	_modulePackage	= Settings::value(Settings::DEVELOPER_FOLDER).toString().toStdString();
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

	QFileInfo descriptionInfo = checkForExistence("description.json", true);
								checkForExistence("icons");
								checkForExistence("qml");
								//checkForExistence("R"); The module is now a package so there is no point in checking for R code because what kind of R package has *no* r-code?

	//Ok everything seems to be in order, let's load!

	QFile descriptionFile(descriptionInfo.absoluteFilePath());
	descriptionFile.open(QFile::ReadOnly);
	std::string	descriptionTxt(descriptionFile.readAll().toStdString());

	parseDescriptionFile(descriptionTxt);

	setStatus(moduleStatus::loadingNeeded);
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
		_requiresDataset				= moduleDescription.get("requiresDataset",	true).asBool();
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

		for(Json::Value & menuEntry : descriptionJson["menu"])
			_menuEntries.push_back(new AnalysisEntry(menuEntry, this));

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

			if(pkgV.isObject())
			{
				out << pkgV["package"].asString();

				if(!pkgV["version"].isNull())
					out << " (>= " << pkgV["version"].asString() << ")";

				first = false;
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

	for(Json::Value & pkgV : _requiredPackages)
		out << standardRIndent << "import('" << ( _requiredPackages.isArray() ? pkgV.asString() : pkgV["package"].asString()) << "');\n";

	return out.str();
}

Json::Value	DynamicModule::requestJsonForPackageInstallationRequest()
{
	if(!installNeeded())
	{
		Log::log() << "DynamicModule::requestJsonForPackageInstallationRequest(): Module (" << _name << ") is already installed!" << std::endl;
	}

	Json::Value requestJson(Json::objectValue);

	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::installNeeded);
	requestJson["moduleName"]		= _name;
	requestJson["moduleCode"]		= generateModuleInstallingR();

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

	ExtractArchive::extractArchiveToFolder(_modulePackage, tmpDir);

	Log::log() << "Unpacked module to folder " << tmpDir << std::endl;

	_modulePackage = modDir;
}

std::string DynamicModule::generateModuleInstallingR()
{
	std::stringstream R;


	if(_modulePackage == "")
	{
		Log::log() << "generateModuleInstallingR has some trouble because package was not unpacked anywehere..." << std::endl;

		setInstallLog("Installing module " + _name + " failed because package filepath was not specified");
		setStatus(moduleStatus::error);
		return "stop('No package specified!')";
	}

	parseDescriptionFile(getDescriptionJsonFromDirectory(_modulePackage));

	generateRPackageMetadata(QDir(_modulePackage.c_str())); //Generate DESCRIPTION and NAMESPACE in case they are missing

	setInstallLog("Installing module " + _name + ".\n");

	std::string typeInstall = "'source'";

	R	<< "libPathsToUse <- c('" << moduleRLibrary().toStdString()	<< "', .libPaths(.Library));\n"
		<< "{\n"
		<< standardRIndent << "withr::with_libpaths(new=libPathsToUse, devtools::install_deps(pkg= '"	<< _modulePackage << "',   lib='" << moduleRLibrary().toStdString() << "'));\n"
		<< standardRIndent << "withr::with_libpaths(new=libPathsToUse, install.packages(pkgs='"			<< _modulePackage << "/.', lib='" << moduleRLibrary().toStdString() << "', type=" << typeInstall << ", repos=NULL));\n"
		<< standardRIndent << "withr::with_libpaths(new=libPathsToUse, find.package(package='" << _name << "'));\n"
		<< "};\n" "return('"+succesResultString()+"');";

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

	std::string analysisTitle = jsonFromJaspFile.get("analysisEntry", "AnalysisEntry's title wasn't actually specified!").asString();

	return retrieveCorrespondingAnalysisEntry(analysisTitle);
}

AnalysisEntry* DynamicModule::retrieveCorrespondingAnalysisEntry(const std::string & codedReference) const
{
	auto parts = stringUtils::splitString(codedReference, '~');

	std::string moduleName		= parts.size() > 1 ? parts[0] : "",
				analysisTitle	= parts.size() > 1 ? parts[1] : parts[0];

	if(!moduleName.empty() && _name != moduleName)
		throw Modules::ModuleException(_name, "This coded reference belongs to a different dynamic module, this one: "+moduleName);

	for (AnalysisEntry * menuEntry : _menuEntries)
		if (menuEntry->isAnalysis() && menuEntry->title() == analysisTitle)
			return menuEntry;

	throw Modules::ModuleException(_name, "Cannot find analysis with title " + analysisTitle + "...");
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

	_status = newStatus;

	emit statusChanged();

	switch(_status)
	{
	case moduleStatus::loadingNeeded:			emit registerForLoading(_name);			break;
	case moduleStatus::installNeeded:			emit registerForInstalling(_name);		break;
	case moduleStatus::error:
		Log::log() << "Just set an error on the status of module "<< _name << std::endl;	break;
	default:																			break;
	}
}

std::string	DynamicModule::moduleNameStripNonAlphaNum(std::string moduleString)
{
	//std::remove_if makes sure all non-ascii chars are removed from your vector, but it does not change the length of the vector. That's why we erase the remaining part of the vector afterwards.
	moduleString.erase(std::remove_if(moduleString.begin(), moduleString.end(), [](unsigned char x)
	{
#ifdef _WIN32
		return !std::isalnum(x, std::locale());
#else
		return !std::isalnum(x);
#endif

	}), moduleString.end());

	return moduleString;
}

void  DynamicModule::setRequiredPackages(Json::Value requiredPackages)
{
	if (_requiredPackages == requiredPackages)
		return;

	_requiredPackages = requiredPackages;
	emit requiredPackagesChanged();
}

void DynamicModule::reloadDescription()
{
	QFile descriptionFile(_moduleFolder.absoluteFilePath() + "/" + nameQ() + "/description.json");

	descriptionFile.open(QIODevice::ReadOnly);

	Json::Value descriptionJson;
	std::string descriptionFileText = descriptionFile.readAll().toStdString();
	if(descriptionFileText.size() == 0) //ignore it when it is empty
		return;

	parseDescriptionFile(descriptionFileText);

	setStatus(moduleStatus::installNeeded);
}

std::string DynamicModule::getDESCRIPTIONFromArchive(const std::string &  filepath)
{
	try {
		return ExtractArchive::extractSingleTextFileFromArchive(filepath, "DESCRIPTION");
	} catch (...) {
		return "";
	}
}

std::string DynamicModule::getDescriptionJsonFromDirectory(const std::string &  filepath)
{

	QString zoekHier = QString::fromStdString(filepath),
			zoekMe   = "description.json";

	return getFileFromDirectory(zoekHier, zoekMe).toStdString();
}

QString DynamicModule::getFileFromDirectory(const QString &  filepath, const QString & searchMe)
{
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
		QString foundThis = getFileFromDirectory(dir.absoluteFilePath(entry), searchMe);
		if(foundThis != "")
			return foundThis;
	}

	return "";
}

std::string DynamicModule::extractPackageNameFromArchive(const std::string & filepath)
{
	std::string DESCRIPTION = getDESCRIPTIONFromArchive(filepath);
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
			//
	}

	if(foundName == "")
	{	//Ok lets try to find description.json then

		std::string descriptionTxt = getDescriptionJsonFromDirectory(filepath);

		if(descriptionTxt != "")
		{
			Json::Value descriptionJson;
			Json::Reader().parse(descriptionTxt, descriptionJson);

			try
			{
				Json::Value & moduleDescription = descriptionJson["moduleDescription"];

				foundName = moduleDescription.get("name", "").asString();

				if(foundName == "")
					foundName = moduleNameStripNonAlphaNum(moduleDescription.get("title", "").asString());
			}
			catch(...)
			{ }
		}
	}

	if(foundName == "") //Then we will just use the archive name and see if that works..
	{
		const std::string	targz	= ".tar.gz",
							tgz		= ".tgz",
							zip		= ".zip";

		std::string fileName = filepath.find_last_of('/') == std::string::npos ? filepath : filepath.substr(filepath.find_last_of('/') + 1);

		auto removeExtension = [&](const std::string & ext){
			if(fileName.size() - ext.size() > 0 && fileName.substr(fileName.size() - ext.size()) == ext)
			{
				foundName = moduleNameStripNonAlphaNum(fileName.substr(0, fileName.size() - ext.size()));
				return false;
			}
			return true;
		};

		removeExtension(targz) || removeExtension(tgz) || removeExtension(zip);
	}

	if(foundName == "") //what the hell?
		throw std::runtime_error("Trying to find package/module name in " + filepath + " but I can't...");

	return foundName;
}

}
