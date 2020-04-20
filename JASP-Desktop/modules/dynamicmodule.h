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
#include "jsonredirect.h"
#include "enginedefinitions.h"
#include "analysisentry.h"

namespace Modules
{

struct ModuleException : public std::runtime_error
{
	ModuleException(std::string moduleName, std::string problemDescription);
};

class DynamicModule : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QString		installLog			READ installLog										NOTIFY installLogChanged		)
	Q_PROPERTY(QString		loadLog				READ loadLog										NOTIFY loadLogChanged			)
	Q_PROPERTY(QString		status				READ status											NOTIFY statusChanged			)
	Q_PROPERTY(bool			loaded				READ loaded				WRITE setLoaded				NOTIFY loadedChanged			)
	Q_PROPERTY(bool			installed			READ installed			WRITE setInstalled			NOTIFY installedChanged			)
	Q_PROPERTY(bool			loading				READ loading			WRITE setLoading			NOTIFY loadingChanged			)
	Q_PROPERTY(bool			installing			READ installing			WRITE setInstalling			NOTIFY installingChanged		)
	Q_PROPERTY(Json::Value	requiredPackages	READ requiredPackages	WRITE setRequiredPackages	NOTIFY requiredPackagesChanged	)
	Q_PROPERTY(bool			initialized			READ initialized		WRITE setInitialized		NOTIFY initializedChanged		)

public:
	///This constructor takes the path to an installed jasp-module as path (aka a directory that contains a library of R packages, one of which is the actual module with QML etc)
	explicit DynamicModule(QString moduleDirectory, QObject *parent);

	///This constructor takes the path to an R-package as first argument, this R-package must also be a jasp-module and will be installed to the app-directory for the particular OS it runs on.
	explicit DynamicModule(std::string modulePackageFile, QObject *parent);

	///This constructor is meant specifically for the development module and only *it*!
	explicit DynamicModule(QObject * parent);

	~DynamicModule() override
	{
		for(auto * entry : _menuEntries)
			delete entry;
		_menuEntries.clear();
	}


	static std::string  developmentModuleName()			{ return _developmentModuleName;	}
	static std::string  defaultDevelopmentModuleName()  { return "DevelopmentModule";		}
	static std::wstring defaultDevelopmentModuleNameW() { return L"DevelopmentModule";		}
	static QString		getJsonDescriptionFilename()	{ return "description.json";		}
	static QFileInfo	developmentModuleFolder();
	static void			developmentModuleFolderCreate();

	std::string			name()				const { return _name;									}
	QString				nameQ()				const { return QString::fromStdString(_name);			}
	std::string			title()				const { return (isDevMod() ? "Dev: " : "") + _title;	}
	bool				requiresData()		const;
	std::string			author()			const { return _author;									}
	std::string			version()			const { return _version;								}
	std::string			website()			const { return _website;								}
	std::string			license()			const { return _license;								}
	std::string			maintainer()		const { return _maintainer;								}
	std::string			description()		const { return _description;							}
	std::string			modulePackage()		const { return _modulePackage;							}

	bool				isDevMod()			const { return _isDeveloperMod;							}
	bool				error()				const { return _status == moduleStatus::error;			}
	bool				readyForUse()		const { return _status == moduleStatus::readyForUse;	}
	bool				installNeeded()		const { return _status == moduleStatus::installNeeded;	}
	bool				loadingNeeded()		const { return _status == moduleStatus::loadingNeeded;	}
	QString				moduleRLibrary()	const { return  _moduleFolder.absolutePath();			}
	Json::Value			requiredPackages()	const { return _requiredPackages;						}

	std::string			qmlFilePath(	const std::string & qmlFileName)	const;
	std::string			iconFilePath(std::string whichIcon = "")			const;
	std::string			iconFolder()										const;
	std::string			rModuleCall(	const std::string & function)		const { return _name + _modulePostFix + "$" + function + _exposedPostFix; }
	QString				helpFolderPath()									const;

	std::string			generateModuleLoadingR(bool shouldReturnSucces = true);
	std::string			generateModuleUnloadingR();
	std::string			generateModuleInstallingR(bool onlyModPkg);
	std::string			generateModuleUninstallingR();

	Json::Value			requestJsonForPackageLoadingRequest();
	Json::Value			requestJsonForPackageUnloadingRequest();
	Json::Value			requestJsonForPackageInstallationRequest(bool onlyModPkg);
	Json::Value			requestJsonForPackageUninstallingRequest();

	void				setInstalled(bool installed);
	void				setLoaded(bool loaded);
	void				setUnloaded();
	void				setLoadingSucces(bool succes);
	void				setInstallingSucces(bool succes);
	void				setLoadingNeeded();
	void				setStatus(moduleStatus newStatus);

	const AnalysisEntries menu()		const	{ return _menuEntries; }

	AnalysisEntry*		retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile)	const;
	AnalysisEntry*		retrieveCorrespondingAnalysisEntry(const std::string & codedReference)		const;
	Json::Value			asJsonForJaspFile(const std::string & function)								const;

	static std::string	succesResultString() { return "succes!"; }

	QString installLog()	const	{ return QString::fromStdString(_installLog);	}
	QString loadLog()		const	{ return QString::fromStdString(_loadLog);		}
	QString status()		const	{ return moduleStatusToQString(_status);		}

	bool shouldUninstallPackagesInRForUninstall();

	bool loaded()		const { return _loaded;		}
	bool installed()	const { return _installed;	}
	bool loading()		const { return _loading;	}
	bool installing()	const { return _installing;	}

	void initialize(); //returns true if install of package(s) should be done
	void parseDescriptionFile(std::string descriptionTxt);

	static QString		getFileFromFolder(				const QString &  filepath,		const QString & searchMe);
	static std::string	getFileFromFolder(				const std::string & folderPath, const std::string & searchMe);
	static std::string	getDESCRIPTIONFromArchive(		const std::string & archivePath);
	static std::string	getDescriptionJsonFromArchive(	const std::string & archivePath);
	static std::string	getDESCRIPTIONFromFolder(		const std::string & folderPath);
	static std::string	getDescriptionJsonFromFolder(	const std::string & folderPath);
	static std::string	extractPackageNameFromArchive(	const std::string & archivePath);
	static std::string	extractPackageNameFromFolder(	const std::string & folderPath);
	static std::string	extractPackageNameFromDESCRIPTIONTxt(const std::string & DESCRIPTION);
	static std::string	extractPackageNameFromDescriptionJsonTxt(const std::string & descriptionJsonTxt);

	void unpackage();
	bool initialized() const { return _initialized;	}

public slots:
	void setInstallLog(std::string installLog);
	void setLoadLog(std::string loadLog);

	void setLoading(bool loading);
	void setInstalling(bool installing);
	void setInitialized(bool initialized);
	void setRequiredPackages(Json::Value requiredPackages);
	void reloadDescription();


signals:
	void installLogChanged();
	void loadLogChanged();
	void statusChanged();
	void loadedChanged(bool loaded);
	void installedChanged(bool installed);
	void loadingChanged(bool loading);
	void installingChanged(bool installing);
	void requiredPackagesChanged();
	void registerForLoading(const std::string & moduleName);
	void registerForInstalling(const std::string & moduleName);
	void registerForInstallingModPkg(const std::string & moduleName);
	void descriptionReloaded(Modules::DynamicModule * dynMod);
	void initializedChanged(bool initialized);

private:
	void		generateRPackageMetadata(QDir packageDir);
	std::string generateNamespaceFileForRPackage();
	std::string generateDescriptionFileForRPackage();

private:
	//QDir			_generatedPackageFolder;
	QFileInfo		_moduleFolder;
	moduleStatus	_status = moduleStatus::initializing;
	std::string		_name,
					_title,
					_icon,
					_author,
					_website,
					_license,
					_loadLog			= "",
					_installLog			= "",
					_maintainer,
					_description,
					_modulePackage		= "",
					_version;
	bool			_installing			= false,
					_installed			= false,
					_loaded				= false,
					_loading			= false,
					_isDeveloperMod		= false,
					_initialized		= false;
	Json::Value		_requiredPackages,
					_previousReqPkgs;
	AnalysisEntries	_menuEntries;
	const char		*_modulePostFix		= "_module",
					*_exposedPostFix	= "_exposed";

	static std::string _developmentModuleName;
};

typedef std::vector<DynamicModule*> DynamicModuleVec;

}

#endif // DYNAMICMODULE_H
