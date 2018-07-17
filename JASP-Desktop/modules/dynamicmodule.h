#ifndef DYNAMICMODULE_H
#define DYNAMICMODULE_H

#include <set>
#include <QDir>
#include <QFile>
#include <sstream>
#include <QObject>
#include <QFileInfo>
#include <QDateTime>
#include "ribbonentry.h"
#include "jsonredirect.h"
#include "enginedefinitions.h"

namespace Modules
{

class DynamicModule : public QObject
{
	Q_OBJECT
public:
	explicit DynamicModule(QString moduleDirectory, QObject *parent) : QObject(parent), _moduleFolder(moduleDirectory) { loadModule(); }

	std::string		name()				const { return _name; }
	bool			error()				const { return _status == moduleStatus::error; }
	bool			readyForUse()		const { return _status == moduleStatus::readyForUse; }
	bool			installNeeded()		const { return _status == moduleStatus::installNeeded; }
	bool			loadingNeeded()		const { return _status == moduleStatus::loadingNeeded; }
	QString			moduleRLibrary()	const { return _moduleFolder.absolutePath() + "/" + _libraryRName + "/"; }
	Json::Value		requiredPackages()	const { return _requiredPackages; }

	Json::Value		requestJsonForPackageLoadingRequest();
	Json::Value		requestJsonForPackageInstallationRequest();


	void		setInstalled(bool succes)	{ _status = succes ? moduleStatus::loadingNeeded	: moduleStatus::error; }
	void		setLoaded(bool succes)		{ _status = succes ? moduleStatus::readyForUse		: moduleStatus::error; }


private:
	void		loadModule();
	void		generateRPackage();
	void		createRLibraryFolder();
	std::string generatedPackageName()					{ return _name+"Pkg"; }
	std::string generateModuleLoadingR();
	std::string generateModuleInstallingR();
	std::string generateNamespaceFileForRPackage();
	std::string generateDescriptionFileForRPackage();


private:
	QDir						_generatedPackageFolder;
	QFileInfo					_moduleFolder;
	int							_version;
	moduleStatus				_status = moduleStatus::installNeeded;
	std::string					_name,
								_title,
								_author,
								_license,
								_maintainer,
								_description;

	Json::Value					_requiredPackages;
	std::vector<RibbonEntry>	_ribbonEntries;
	const char *				_libraryRName = "libraryR";

};

}

#endif // DYNAMICMODULE_H
