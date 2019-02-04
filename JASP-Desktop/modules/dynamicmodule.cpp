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

namespace Modules
{

ModuleException::ModuleException(std::string moduleName, std::string problemDescription)
	: std::runtime_error("Module " + moduleName + " had a problem: " + problemDescription)
{}

const char * standardRIndent = "  ";

bool DynamicModule::initialize()
{
	//Check some stuff
	_moduleFolder.makeAbsolute();
	QDir moduleDir(_moduleFolder.absoluteDir());

	_name = moduleNameFromFolder(moduleDir.dirName().toStdString());

	if(!_moduleFolder.exists())				throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " does not exist!");
	else if(!_moduleFolder.isDir())			throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " is not a directory!");
	else if(!_moduleFolder.isWritable())	throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " is not writable!");


	auto checkForExistence = [&](std::string name, bool isFile = false)
	{
		QFileInfo checkInfo(_moduleFolder.absolutePath() + "/" + QString::fromStdString(name));

		std::string errorMsg = "";

		if(!checkInfo.exists())	errorMsg = name + " is missing from " + _moduleFolder.absolutePath().toStdString();

		if(errorMsg == "" && !isFile	&& !checkInfo.isDir())	errorMsg = name + " is not, as expected, a directory";
		if(errorMsg == "" &&  isFile	&& !checkInfo.isFile())	errorMsg = name + " is not, as expected, a file";

		if(errorMsg != "")
		{
/*#ifdef JASP_DEBUG
			std::cout << "Checking module folder gave error: " << errorMsg << " but this is not necessarily a problem, so dont worry." << std::endl;
#endif*/
			throw std::runtime_error(errorMsg);
		}

		return checkInfo;
	};

	QFileInfo descriptionInfo = checkForExistence("description.json", true);
								checkForExistence("icons");
								checkForExistence("qml");
								checkForExistence("R");

	_generatedPackageFolder = QDir(_moduleFolder.absolutePath() + "/" + QString::fromStdString(generatedPackageName()));

	//Ok everything seems to be in order, let's load!

	QFile descriptionFile(descriptionInfo.absoluteFilePath());
	descriptionFile.open(QFile::ReadOnly);
	std::string	descriptionTxt(descriptionFile.readAll().toStdString());

	Json::Value descriptionJson;
	Json::Reader().parse(descriptionTxt, descriptionJson);

	try
	{
		Json::Value & moduleDescription = descriptionJson["moduleDescription"];
		_title							= moduleDescription.get("title",			_name).asString();
		_author							= moduleDescription.get("author",			"Unknown").asString();
		_requiresDataset				= moduleDescription.get("requiresDataset",	true).asBool();
		_license						= moduleDescription.get("license",			"Unknown").asString();
		_website						= moduleDescription.get("website",			"Unknown").asString();
		_maintainer						= moduleDescription.get("maintainer",		"JASP Team <info@jasp-stats.org>").asString();
		_description					= moduleDescription.get("description",		"The R Code belonging to module " + _name).asString();
		_version						= moduleDescription.get("version",			0).asInt();
		_requiredPackages				= descriptionJson["requiredPackages"]; //can be sent straight to engine later on!

		for(Json::Value & ribbonEntry : descriptionJson["ribbonEntries"])
			_ribbonEntries.push_back(new RibbonEntry(ribbonEntry, this));
	}
	catch(std::exception e)
	{
		throw std::runtime_error("During the parsing of the description.json of the Module " + _name + " something went wrong: " + e.what());
	}

	bool shouldInstall = false;

	try { checkForExistence(_libraryRName);				} catch(...) {	createRLibraryFolder();	shouldInstall = true; }
	try { checkForExistence(generatedPackageName());	} catch(...) {	generateRPackage();		shouldInstall = true; }

	return shouldInstall;
}

void DynamicModule::setLoadingNeeded()
{
	if(_status != moduleStatus::installNeeded)
		_status = moduleStatus::loadingNeeded;
}

void DynamicModule::createRLibraryFolder()
{
	QDir	libDir(moduleRLibrary()),
			moduleDir(_moduleFolder.absolutePath());
	moduleDir.mkdir(_libraryRName);

	if(!libDir.exists())
		throw std::runtime_error("Failed creating library directory for Module ("+_name+")");
}

void DynamicModule::generateRPackage()
{
	QDir	moduleDir(	_moduleFolder.absolutePath()),
			packageDir(	_generatedPackageFolder),
			origRDir(	_moduleFolder.absolutePath()	+ "/R"),
			newRDir(	packageDir.absolutePath()		+ "/R");

	moduleDir.mkdir(packageDir.dirName());
	packageDir.mkdir(newRDir.dirName());

	for(QString rFileName : newRDir.entryList(QDir::Files))
		QFile::remove(newRDir.absoluteFilePath(rFileName));

	for(QString rFileName : origRDir.entryList(QDir::Files))
		QFile::copy(origRDir.absoluteFilePath(rFileName), newRDir.absoluteFilePath(rFileName));


	QFile	descriptionFile(packageDir.absoluteFilePath("DESCRIPTION")),
			namespaceFile(	packageDir.absoluteFilePath("NAMESPACE"));

	descriptionFile.open(QFile::WriteOnly	| QFile::Truncate);
	namespaceFile.open(QFile::WriteOnly		| QFile::Truncate);

	descriptionFile.write(generateDescriptionFileForRPackage().c_str());
	namespaceFile.write(generateNamespaceFileForRPackage().c_str());
}

std::string DynamicModule::generateDescriptionFileForRPackage()
{
	std::stringstream out;

	out << "Package: " << generatedPackageName() <<
		"\nType: Package"
		"\nTitle: A generated package for JASP's " << _name << " Module"
		"\nVersion: "		<< std::to_string(_version) << ".0"
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
			out << pkgV["package"].asString();

			if(!pkgV["version"].isNull())
				out << " (>= " << pkgV["version"].asString() << ")";

			first = false;
		}
	}

	return out.str();

}

std::string DynamicModule::generateNamespaceFileForRPackage()
{
	std::stringstream out;

	for(RibbonEntry * ribbon : _ribbonEntries)
		for(const AnalysisEntry * analysis : ribbon->analysisEntries())
			out << "export(" << analysis->function() << ")\n";

	for(Json::Value & pkgV : _requiredPackages)
		out << standardRIndent << "import('" << pkgV["package"].asString() << "');\n";

	return out.str();
}

Json::Value	DynamicModule::requestJsonForPackageInstallationRequest()
{
	if(!installNeeded())
		std::cout << "DynamicModule::requestJsonForPackageInstallationRequest(): Module (" << _name << ") is already installed!";

	Json::Value requestJson(Json::objectValue);

	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::installNeeded);
	requestJson["moduleName"]		= _name;
	requestJson["moduleCode"]		= generateModuleInstallingR();


	return requestJson;
}

Json::Value	DynamicModule::requestJsonForPackageLoadingRequest()
{
	Json::Value requestJson(Json::objectValue);

	requestJson["moduleRequest"]	= moduleStatusToString(moduleStatus::loadingNeeded);
	requestJson["moduleName"]		= _name;
	requestJson["moduleCode"]		= generateModuleLoadingR();

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

std::string DynamicModule::generateModuleInstallingR()
{

	std::set<std::string> pkgsVersionless;

	if(_requiredPackages.isArray())
		for(Json::Value & pkgV : _requiredPackages)
			if(pkgV["version"].isNull())
				pkgsVersionless.insert(pkgV["package"].asString());

	std::stringstream R, installLog;

	//out << "print(paste('ping=', pingr::ping('cloud.r-project.org', count=2)));\n";
	//out <<" print(Sys.getenv());\n";

	R			<< "libPathsToUse <- c(.libPaths(), '" << moduleRLibrary().toStdString() << "');\n"  "{\n"; //"print(libPathsToUse);\n"
	installLog	<< "Installing module " << _name << ", with required packages: ";

	const	bool useWithLibPaths	= true;
			bool firstPkg			= true;

	if(pkgsVersionless.size() > 0)
	{
		R			<< standardRIndent << (useWithLibPaths ? "withr::with_libpaths(new=libPathsToUse,  " : "") << "install.packages(repos='https://cloud.r-project.org', Ncpus=4, lib='" << moduleRLibrary().toStdString() << "', pkgs=c(";

		int count = 0;
		for(const std::string & pkg : pkgsVersionless)
		{
			R			<< (count++ > 0 ? std::string(",\n") + standardRIndent + standardRIndent : "") << "'" << pkg << "'";
			installLog	<< (!firstPkg   ? ", " : "") << pkg;

			firstPkg = false;
		}

		R			<< ")" << (useWithLibPaths ? ")" : "") <<  ");\n";
		installLog	<< "\n";
	}

	if(_requiredPackages.isArray())
		for(Json::Value & pkgV : _requiredPackages)
			if(!pkgV["version"].isNull())
			{
				R			<< standardRIndent << (useWithLibPaths ? "withr::with_libpaths(new=libPathsToUse,  " : "") << "devtools::install_version(repos='https://cloud.r-project.org', type='binary', Ncpus=4, package='" << pkgV["package"].asString() << "', version='" << pkgV["version"].asString() << "', lib='" << moduleRLibrary().toStdString() << "', args='--library=\"" << moduleRLibrary().toStdString() << "\"')" << (useWithLibPaths ? ")" : "") <<  ";\n";

				installLog	<< (!firstPkg   ? ", " : "") << pkgV["package"].asString() << " (" <<  pkgV["version"].asString() << ")";

				firstPkg = false;
			}

	if(firstPkg)
		installLog << "none";

	installLog << ".\n";

	setInstallLog(QString::fromStdString(installLog.str()));

	R << standardRIndent << (useWithLibPaths ? "withr::with_libpaths(new=libPathsToUse,  " : "");
	R << "install.packages(repos=NULL, pkgs='" << _generatedPackageFolder.absolutePath().toStdString() << "', lib='" << moduleRLibrary().toStdString() << "', type='source')" << (useWithLibPaths ? ")" : "") <<  ";" "\n" "}\n" "return('"+succesResultString()+"')";

#ifdef JASP_DEBUG
	std::cout << "DynamicModule(" << _name << ")::generateModuleInstallingR() generated:\n" << R.str() << std::endl;
#endif

	return R.str();
}

std::string DynamicModule::generateModuleLoadingR(bool shouldReturnSucces)
{
	std::stringstream R;

	setLoadLog("Module " + QString::fromStdString(_name) + " is being loaded from " + _moduleFolder.absolutePath() + "\n");

	R << _name << " <- module({\n" << standardRIndent << ".libPaths('" << moduleRLibrary().toStdString() << "');\n";
	R << standardRIndent << "import('" << generatedPackageName() << "');\n\n";

	for(RibbonEntry * ribbon : _ribbonEntries)
		for(const AnalysisEntry * analysis : ribbon->analysisEntries())
			R << standardRIndent << analysis->function() << _exposedPostFix << " <- function(...) " << analysis->function() << "(...)\n";
	R << "})\n";

	if(shouldReturnSucces)
		R << "return('"+succesResultString()+"')";


#ifdef JASP_DEBUG
	std::cout << "DynamicModule(" << _name << ")::generateModuleLoadingR() generated:\n" << R.str() << std::endl;
#endif

	return R.str();
}

std::string DynamicModule::generateModuleUnloadingR()
{
	std::stringstream out;

	out << _name << " <- NULL; gc(); return('"+succesResultString()+"')";


#ifdef JASP_DEBUG
	std::cout << "DynamicModule(" << _name << ")::generateModuleUnloadingR() generated:\n" << out.str() << std::endl;
#endif

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
	return _moduleFolder.absolutePath().toStdString() + "/qml/" + qmlFileName;
}

std::string	DynamicModule::iconFilePath(const std::string & iconFileName)	const
{
	return _moduleFolder.absolutePath().toStdString() + "/icons/" + iconFileName;
}

RibbonEntry* DynamicModule::ribbonEntry(const std::string & ribbonTitle) const
{
	for(RibbonEntry * entry : _ribbonEntries)
		if(entry->title() == ribbonTitle)
			return entry;

	throw ModuleException(name(), "Couldn't find RibbonEntry " + ribbonTitle);
}

AnalysisEntry* DynamicModule::retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile) const
{
	std::string moduleName		= jsonFromJaspFile.get("moduleName", "Modulename wasn't actually filled!").asString();
	int			moduleVersion	= jsonFromJaspFile.get("moduleVersion", -1).asInt();

	if(moduleName != name())
		throw ModuleException(name(), "Tried to load an AnalysisEntry for module (" + moduleName +") from me...");

	if(moduleVersion != version())
		std::cerr << "Loading analysis based on different version of module(" << moduleName << "), but going ahead anyway. Analysis based on version: " << moduleVersion << " and actual loaded version of module is: " << version() << std::endl;

	std::string ribbonTitle = jsonFromJaspFile.get("ribbonEntry", "RibbonEntry's title wasn't actually specified!").asString();

	return ribbonEntry(ribbonTitle)->retrieveCorrespondingAnalysisEntry(jsonFromJaspFile);
}

AnalysisEntry*  DynamicModule::retrieveCorrespondingAnalysisEntry(const std::string & ribbonTitle, const std::string & analysisTitle) const
{
	return ribbonEntry(ribbonTitle)->analysisEntry(analysisTitle);
}

void DynamicModule::setInstallLog(QString installLog)
{
	if (_installLog == installLog.toStdString())
		return;

	_installLog = installLog.toStdString();
	emit installLogChanged();
}

void DynamicModule::setLoadLog(QString loadLog)
{
	if (_loadLog == loadLog.toStdString())
		return;

	_loadLog = loadLog.toStdString();
	emit loadLogChanged();
}

void DynamicModule::setInstalled(bool succes)
{
	_status = succes ? moduleStatus::loadingNeeded	: moduleStatus::error;

	setInstallLog(installLog() + "Installation " + (succes ? "succeeded" : "failed") + "\n");
}

void DynamicModule::setLoaded(bool succes)
{
	_status = succes ? moduleStatus::readyForUse		: moduleStatus::error;
	setLoadLog(loadLog() + "Loading " + (succes ? "succeeded" : "failed") + "\n");
}

}
