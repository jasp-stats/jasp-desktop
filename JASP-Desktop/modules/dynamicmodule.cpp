#include "dynamicmodule.h"

namespace Modules
{

void DynamicModule::loadModule()
{
	//Check some stuff

	_moduleFolder.makeAbsolute();
	QDir moduleDir(_moduleFolder.absoluteDir());

	_name = moduleDir.dirName().toStdString();
	_name.erase(std::remove(_name.begin(), _name.end(), ' '), _name.end()); //remove spaces

	if(!_moduleFolder.exists())				throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " does not exist!");
	else if(!_moduleFolder.isDir())			throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " is not a directory!");
	else if(!_moduleFolder.isWritable())	throw std::runtime_error(_moduleFolder.absolutePath().toStdString() + " is not writable!");


	auto checkForExistence = [&](QString name, bool isFile = false)
	{
		QFileInfo checkInfo(_moduleFolder.absolutePath() + "/" + name);
		if(!checkInfo.exists())	throw std::runtime_error(name.toStdString() + " is missing from " + _moduleFolder.absolutePath().toStdString());

		if(!isFile	&& !checkInfo.isDir())	throw std::runtime_error(name.toStdString() + " is not, as expected, a directory");
		if(isFile	&& !checkInfo.isFile())	throw std::runtime_error(name.toStdString() + " is not, as expected, a file");

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
		_title							= moduleDescription.get("title",		_name).asString();
		_author							= moduleDescription.get("author",		"Unknown").asString();
		_license						= moduleDescription.get("license",		"Unknown").asString();
		_maintainer						= moduleDescription.get("maintainer",	"JASP Team <info@jasp-stats.org>").asString();
		_description					= moduleDescription.get("description",	"The R Code belonging to module" + _name).asString();
		_version						= moduleDescription.get("version",		0).asInt();
		_requiredPackages				= descriptionJson["requiredPackages"]; //can be sent straight to engine later on!

		for(Json::Value & ribbonEntry : descriptionJson["ribbonEntries"])
			_ribbonEntries.push_back(RibbonEntry(ribbonEntry));
	}
	catch(std::exception e)
	{
		throw std::runtime_error("During the parsing of the description.json of the Module " + _name + " something went wrong: " + e.what());
	}

	createRLibraryFolder();
	generateRPackage();
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
		QFile::remove(rFileName);

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

	for(RibbonEntry & ribbon : _ribbonEntries)
		for(const AnalysisEntry & analysis : ribbon.analysisEntries())
			out << "export(" << analysis.function() << ")\n";

	return out.str();
}

Json::Value	DynamicModule::requestJsonForPackageInstallationRequest()
{
	if(!installNeeded())
		throw std::runtime_error("Module (" + _name + ") is already installed!");

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

const char * standardRIndent = "  ";

std::string DynamicModule::generateModuleInstallingR()
{

	std::set<std::string> pkgsVersionless;

	if(_requiredPackages.isArray())
		for(Json::Value & pkgV : _requiredPackages)
			if(pkgV["version"].isNull())
				pkgsVersionless.insert(pkgV["package"].asString());

	std::stringstream out;

	out << "libPathsToUse <- c(.libPaths(), '" << moduleRLibrary().toStdString() << "');\n" "{\n";

	if(pkgsVersionless.size() > 0)
	{
		out << standardRIndent << "install.packages(repos='https://cloud.r-project.org', Ncpus=4, lib='" << moduleRLibrary().toStdString() << "', pkgs=c(";

		int count = 0;
		for(const std::string & pkg : pkgsVersionless)
			out << (count++ > 0 ? std::string(",\n") + standardRIndent + standardRIndent : "") << "'" << pkg << "'";

		out << "));\n";
	}

	if(_requiredPackages.isArray())
		for(Json::Value & pkgV : _requiredPackages)
			if(!pkgV["version"].isNull())
				out << standardRIndent << "withr::with_libpaths(new=libPathsToUse, devtools::install_version(repos='https://cloud.r-project.org', Ncpus=4, package='" << pkgV["package"].asString() << "', version='" << pkgV["version"].asString() << "', lib='" << moduleRLibrary().toStdString() << "', args='--library=\"" << moduleRLibrary().toStdString() << "\"'));\n";

	out << standardRIndent << "install.packages(repos=NULL, pkgs='" << _generatedPackageFolder.absolutePath().toStdString() << "', lib='" << moduleRLibrary().toStdString() << "', type='source');" "\n" "}\n" "return('succes!')";

#ifdef JASP_DEBUG
	std::cout << "DynamicModule(" << _name << ")::generateModuleInstallingR() generated:\n" << out.str() << std::endl;
#endif

	return out.str();
}

std::string DynamicModule::generateModuleLoadingR()
{
	std::stringstream out;

	out << _name << " <- module({\n" << standardRIndent << ".libPaths('" << moduleRLibrary().toStdString() << "');\n";

	for(Json::Value & pkgV : _requiredPackages)
		out << standardRIndent << "import('" << pkgV["package"].asString() << "');\n";

	out << standardRIndent << "import('" << generatedPackageName() << "');\n\n";

	for(RibbonEntry & ribbon : _ribbonEntries)
		for(const AnalysisEntry & analysis : ribbon.analysisEntries())
			out << standardRIndent << analysis.function() << " <- function(...) " << analysis.function() << "(list(...))\n";
	out << "})\n" "return('succes!')";


#ifdef JASP_DEBUG
	std::cout << "DynamicModule(" << _name << ")::generateModuleLoadingR() generated:\n" << out.str() << std::endl;
#endif

	return out.str();
}

}
