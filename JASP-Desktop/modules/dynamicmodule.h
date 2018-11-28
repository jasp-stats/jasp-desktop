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
#include "utilities/appdirs.h"

namespace Modules
{

struct ModuleException : public std::runtime_error
{
	ModuleException(std::string moduleName, std::string problemDescription) : std::runtime_error("Module " + moduleName + " had a problem: " + problemDescription) {}
};

class DynamicModule : public QObject
{
	Q_OBJECT
public:
	explicit DynamicModule(QString moduleDirectory, QObject *parent) : QObject(parent), _moduleFolder(moduleDirectory)
	{
		_status = loadModule() ? moduleStatus::installNeeded : moduleStatus::loadingNeeded;
	}

	~DynamicModule()
	{
		for(auto * entry : _ribbonEntries)
			delete entry;
		_ribbonEntries.clear();
	}



	std::string		name()				const { return _name;				}
	std::string		title()				const { return _title;				}
	bool			requiresDataset()	const { return _requiresDataset;	}
	std::string		author()			const { return _author;				}
	int				version()			const { return _version;			}
	std::string		website()			const { return _website;			}
	std::string		license()			const { return _license;			}
	std::string		maintainer()		const { return _maintainer;			}
	std::string		description()		const { return _description;		}

	bool			error()				const { return _status == moduleStatus::error;			}
	bool			readyForUse()		const { return _status == moduleStatus::readyForUse;	}
	bool			installNeeded()		const { return _status == moduleStatus::installNeeded;	}
	bool			loadingNeeded()		const { return _status == moduleStatus::loadingNeeded;	}
	QString			moduleRLibrary()	const { return  _moduleFolder.absolutePath() + "/" + _libraryRName + "/"; }
	Json::Value		requiredPackages()	const { return _requiredPackages; }

	Json::Value		requestJsonForPackageLoadingRequest();
	Json::Value		requestJsonForPackageInstallationRequest();


	void		setInstalled(bool succes)	{ _status = succes ? moduleStatus::loadingNeeded	: moduleStatus::error; }
	void		setLoaded(bool succes)		{ _status = succes ? moduleStatus::readyForUse		: moduleStatus::error; }

	std::string	qmlFilePath(std::string qmlFileName)	const;
	std::string	rModuleCall(std::string function)		const { return _name + "$" + function + _exposedPostFix; }
	const std::vector<RibbonEntry*> ribbonEntries()		const	{ return _ribbonEntries; }

	AnalysisEntry* firstAnalysisEntry(); //Just for testing
	AnalysisEntry* retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile);

	static std::string moduleNameFromFolder(std::string folderName) { folderName.erase(std::remove(folderName.begin(), folderName.end(), ' '), folderName.end());  return folderName;}

	std::string generatedPackageName()					{ return _name + "Pkg"; }

private:
	bool		loadModule(); //returns true if install of package(s) should be done
	void		generateRPackage();
	void		createRLibraryFolder();
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
								_website,
								_license,
								_maintainer,
								_description;

	bool						_requiresDataset = true;

	Json::Value					_requiredPackages;
	std::vector<RibbonEntry*>	_ribbonEntries;
	const char					*_libraryRName = "libraryR",
								*_exposedPostFix = "_exposed";


};

}

#endif // DYNAMICMODULE_H
