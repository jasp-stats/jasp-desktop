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
	ModuleException(std::string moduleName, std::string problemDescription);
};

class DynamicModule : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QString		installLog			READ installLog			WRITE setInstallLog			NOTIFY installLogChanged		)
	Q_PROPERTY(QString		loadLog				READ loadLog			WRITE setLoadLog			NOTIFY loadLogChanged			)
	Q_PROPERTY(QString		status				READ status											NOTIFY statusChanged			)
	Q_PROPERTY(bool			loaded				READ loaded				WRITE setLoaded				NOTIFY loadedChanged			)
	Q_PROPERTY(bool			installed			READ installed			WRITE setInstalled			NOTIFY installedChanged			)
	Q_PROPERTY(bool			loading				READ loading			WRITE setLoading			NOTIFY loadingChanged			)
	Q_PROPERTY(bool			installing			READ installing			WRITE setInstalling			NOTIFY installingChanged		)
	Q_PROPERTY(Json::Value	requiredPackages	READ requiredPackages	WRITE setRequiredPackages	NOTIFY requiredPackagesChanged	)

public:
	explicit DynamicModule(QString moduleDirectory, QObject *parent) : QObject(parent), _moduleFolder(moduleDirectory)
	{
		QDir moduleDir(_moduleFolder.absoluteDir());
		_name = moduleNameFromFolder(moduleDir.dirName().toStdString());
	}

	~DynamicModule() override
	{
		for(auto * entry : _ribbonEntries)
			delete entry;
		_ribbonEntries.clear();
	}

	std::string			name()				const { return _name;				}
	std::string			title()				const { return _title;				}
	bool				requiresDataset()	const { return _requiresDataset;	}
	std::string			author()			const { return _author;				}
	int					version()			const { return _version;			}
	std::string			website()			const { return _website;			}
	std::string			license()			const { return _license;			}
	std::string			maintainer()		const { return _maintainer;			}
	std::string			description()		const { return _description;		}

	bool				error()				const { return _status == moduleStatus::error;			}
	bool				readyForUse()		const { return _status == moduleStatus::readyForUse;	}
	bool				installNeeded()		const { return _status == moduleStatus::installNeeded || _status == moduleStatus::installModPkgNeeded || _status == moduleStatus::installReqPkgsNeeded;	}
	bool				loadingNeeded()		const { return _status == moduleStatus::loadingNeeded;	}
	QString				moduleRLibrary()	const { return  _moduleFolder.absolutePath() + "/" + _libraryRName; }
	Json::Value			requiredPackages()	const { return _requiredPackages; }

	std::string			generatedPackageName()								const { return _name + "Pkg"; }
	std::string			qmlFilePath(	const std::string & qmlFileName)	const;
	std::string			iconFilePath(	const std::string & iconFileName)	const;
	std::string			rModuleCall(	const std::string & function)		const { return _name + "$" + function + _exposedPostFix; }

	std::string			generateModuleLoadingR(bool shouldReturnSucces = true);
	std::string			generateModuleUnloadingR();
	std::string			generateModuleInstallingR(bool installRequiredPackages = true, bool installModulePkg = true);
	std::string			generateModuleUninstallingR();

	Json::Value			requestJsonForPackageLoadingRequest();
	Json::Value			requestJsonForPackageUnloadingRequest();
	Json::Value			requestJsonForPackageInstallationRequest();
	Json::Value			requestJsonForPackageUninstallingRequest();

	void				setInstalled(bool installed);
	void				setLoaded(bool loaded);
	void				setUnloaded();
	void				setLoadingSucces(bool succes);
	void				setInstallingSucces(bool succes);
	void				setLoadingNeeded();
	void				setStatus(moduleStatus newStatus);

	const RibbonEntries ribbonEntries()		const	{ return _ribbonEntries; }

	RibbonEntry*		ribbonEntry(const std::string & ribbonTitle) const;
	RibbonEntry*		operator[](const std::string & ribbonTitle) const { return ribbonEntry(ribbonTitle); }

	AnalysisEntry*		retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile)								const;
	AnalysisEntry*		retrieveCorrespondingAnalysisEntry(const std::string & codedReference)									const;
	AnalysisEntry*		retrieveCorrespondingAnalysisEntry(const std::string & ribbonTitle, const std::string & analysisName)	const;

	static std::string	moduleNameFromFolder(std::string folderName);

	static std::string	succesResultString() { return "succes!"; }

	QString installLog()	const	{ return QString::fromStdString(_installLog);	}
	QString loadLog()		const	{ return QString::fromStdString(_loadLog);		}
	QString status()		const	{ return moduleStatusToQString(_status);		}

	bool shouldUninstallPackagesInRForUninstall();

	bool loaded()		const { return _loaded;		}
	bool installed()	const { return _installed;	}
	bool loading()		const { return _loading;	}
	bool installing()	const { return _installing;	}

	void setInstallModulePackageNeeded();
	void setInstallRequiredPackagesNeeded();
	void regenerateModulePackage();
	void initialize(); //returns true if install of package(s) should be done

public slots:
	void setInstallLog(QString installLog);
	void setLoadLog(QString loadLog);

	void setLoading(bool loading);
	void setInstalling(bool installing);
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
	void descriptionReloaded(Modules::DynamicModule * dynMod);

private:
	void		generateRPackage();
	void		createRLibraryFolder();
	std::string generateNamespaceFileForRPackage();
	std::string generateDescriptionFileForRPackage();

private:
	QDir			_generatedPackageFolder;
	QFileInfo		_moduleFolder;
	int				_version;
	moduleStatus	_status = moduleStatus::uninitialized;
	std::string		_name,
					_title,
					_author,
					_website,
					_license,
					_maintainer,
					_description,
					_installLog	= "",
					_loadLog	= "";

	bool			_requiresDataset	= true,
					_loaded				= false,
					_installed			= false,
					_loading			= false,
					_installing			= false;
	Json::Value		_requiredPackages;
	RibbonEntries	_ribbonEntries;
	const char		*_libraryRName		= "libraryR",
					*_exposedPostFix	= "_exposed";
};

typedef std::vector<DynamicModule*> DynamicModuleVec;

}

#endif // DYNAMICMODULE_H
