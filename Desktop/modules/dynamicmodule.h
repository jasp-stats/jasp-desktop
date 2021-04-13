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
#include "version.h"
#include <QFileInfo>
#include <QDateTime>
#include <QQmlEngine>
#include "jsonredirect.h"
#include "analysisentry.h"
#include "utilities/qutils.h"
#include "enginedefinitions.h"
#include "upgrader/upgradeDefinitions.h"

namespace Modules
{

typedef std::set<std::string> stringset;

struct ModuleException : public std::runtime_error
{
	ModuleException(std::string moduleName, std::string problemDescription);
	
	std::string moduleName, problemDescription;
};

class Description;
class Upgrades;

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
	Q_PROPERTY(bool			initialized			READ initialized		WRITE setInitialized		NOTIFY initializedChanged		)
	Q_PROPERTY(bool			isBundled			READ isBundled			WRITE setBundled			NOTIFY bundledChanged			)
	Q_PROPERTY(QStringList	importsR			READ importsRQ										NOTIFY importsRChanged			)
	Q_PROPERTY(bool			error				READ error											NOTIFY errorChanged				)

public:
	//To do make the constructors less misleading (std::string vs QString does not do the same thing at all!) Some kind of a static MakeDynamicModule function and making the constructors private should do the trick
	///This constructor takes the path to an installed jasp-module as path (aka a directory that contains a library of R packages, one of which is the actual module with QML etc)
	explicit DynamicModule(QString moduleDirectory, QObject *parent, bool isBundled, bool isCommon);

	///This constructor takes the path to an R-package as first argument, this R-package must also be a jasp-module and will be installed to the app-directory for the particular OS it runs on.
	explicit DynamicModule(std::string modulePackageFile, QObject *parent, bool unpack = true);

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
	static std::string	getQmlDescriptionFilename()		{ return "Description.qml";			}
	static std::string	getQmlUpgradesFilename()		{ return "Upgrades.qml";			}
	static QFileInfo	developmentModuleFolder();
	static void			developmentModuleFolderCreate();
	static bool			isDescriptionFile(const std::string & filename);
	static bool			isDescriptionFile(const QString		& filename);

	std::string			name()				const { return _name;									}
	QString				nameQ()				const { return QString::fromStdString(name());			}
	std::string			title()				const { return (isDevMod() ? "Dev: " : "") + _title;	}
	bool				requiresData()		const;
	std::string			author()			const { return _author;									}
	std::string			version()			const { return _version;								}
	QString				versionQ()			const { return QString::fromStdString(_version);		}
	std::string			website()			const { return _website;								}
	std::string			license()			const { return _license;								}
	std::string			maintainer()		const { return _maintainer;								}
	std::string			description()		const { return _descriptionTxt;							}
	std::string			modulePackage()		const { return _modulePackage;							}
	bool				isCommon()			const { return _isCommon;								}

	bool				isDevMod()			const { return _isDeveloperMod;							}
	bool				error()				const { return _status == moduleStatus::error;			}
	bool				readyForUse()		const { return _status == moduleStatus::readyForUse;	}
	bool				installNeeded()		const { return _status == moduleStatus::installNeeded;	}
	bool				loadingNeeded()		const { return _status == moduleStatus::loadingNeeded;	}
	QString				moduleRLibrary()	const { return  _moduleFolder.absolutePath();			}
	const stringset &	importsR()			const { return _importsR;						}
	QStringList			importsRQ()			const { return tql(_importsR);					}
	stringset			requiredModules()	const;
	
	std::string			getLibPathsToUse();

	bool				requiresModule(const std::string & moduleName) { return _importsR.count(moduleName) > 0; }
	

	std::string			moduleInstFolder()									const; //Where is the "inst" folder?
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
	void				setReadyForUse();
	void				setStatus(moduleStatus newStatus);

	const AnalysisEntries & menu()		const	{ return _menuEntries; }

	AnalysisEntry*		retrieveCorrespondingAnalysisEntry(const Json::Value & jsonFromJaspFile)	const;
	AnalysisEntry*		retrieveCorrespondingAnalysisEntry(const std::string & codedReference)		const;
	Json::Value			asJsonForJaspFile(const std::string & function)								const;

	static std::string	succesResultString() { return "succes!"; }

	QString installLog()	const	{ return QString::fromStdString(_installLog);	}
	QString loadLog()		const	{ return QString::fromStdString(_loadLog);		}
	QString status()		const	{ return moduleStatusToQString(_status);		}

	bool shouldUninstallPackagesInRForUninstall();

	bool loaded()		const { return _loaded;			}
	bool installed()	const { return _installed;		}
	bool loading()		const { return _loading;		}
	bool installing()	const { return _installing;		}
	bool initialized()	const { return _initialized;	}
	bool isBundled()	const { return _bundled;		}

	void initialize(); //returns true if install of package(s) should be done
	void loadDescriptionQml(const QString		& descriptionTxt,	const QUrl		& url);
	void loadUpgradesQML(	const QString		& upgradesTxt,		const QUrl		& url);
	bool hasUpgradesToApply(const std::string	& function,			const Version	& version);
	void applyUpgrade(		const std::string	& function,			const Version	& version, Json::Value & analysesJson, UpgradeMsgs & msgs, StepsTaken & stepsTaken);

	void loadDescriptionFromFolder(									const std::string & folderPath);
	void loadDescriptionFromArchive(								const std::string & archivePath);
	void loadRequiredModulesFromFolder(								const std::string & folderPath)			{ loadRequiredModulesFromDESCRIPTIONTxt( tq( getDESCRIPTIONFromFolder ( folderPath  ) ) ); }
	void loadRequiredModulesFromArchive(							const std::string & archivePath)		{ loadRequiredModulesFromDESCRIPTIONTxt( tq( getDESCRIPTIONFromArchive( archivePath ) ) ); }
	void loadRequiredModulesFromDESCRIPTIONTxt(						const QString	  & DESCRIPTION);
	static QString		getFileFromFolder(							const QString     & filepath,	const QString     & searchMe);
	static std::string	getFileFromFolder(							const std::string & folderPath, const std::string & searchMe);
	static std::string	getDESCRIPTIONFromArchive(					const std::string & archivePath);
	static std::string	getDESCRIPTIONFromFolder(					const std::string & folderPath);
	static std::string	getDescriptionQmlFromArchive(				const std::string & archivePath);
	static std::string	getDescriptionQmlFromFolder(				const std::string & folderPath);
	static std::string	extractPackageNameFromArchive(				const std::string & archivePath);
	static std::string	extractPackageNameFromFolder(				const std::string & folderPath);
	static std::string	extractPackageNameFromDESCRIPTIONTxt(		const std::string & DESCRIPTION);
	static std::string	extractPackageNameFromDescriptionQmlTxt(	const std::string & descriptionQmlTxt);
	static std::string	extractPackageNameFromDescriptionJsonTxt(	const std::string & descriptionJsonTxt);

	void unpackage();
	///Make sure url ends with the actual filename of the qml you are loading, otherwise translations will not work! Also make it with QUrl::fromLocalFile otherwise Windows messes things up
	static Description	* instantiateDescriptionQml(const QString & descriptionTxt, const QUrl & url, const std::string & moduleName);
	static Upgrades		* instantiateUpgradesQml(	const QString & upgradesTxt,	const QUrl & url, const std::string & moduleName);
	static QObject		* instantiateQml(			const QString & qml,			const QUrl & url, const std::string & moduleName, const std::string & whatAmILoading, const std::string & filename);

	std::string toString();
	void loadInfoFromDescriptionItem(Description * description);

public slots:
	void reloadDescription();
	void setLoading(			bool		loading);
	void setInstalling(			bool		installing);
	void setInitialized(		bool		initialized);
	void setBundled(			bool		isBundled);
	void setInstallLog(			std::string installLog);
	void setLoadLog(			std::string loadLog);
	void setImportsR(	stringset	importsR);

signals:
	void		installLogChanged();
	void		loadLogChanged();
	void		statusChanged();
	void		requiredPackagesChanged();
	void		loadedChanged(		bool loaded);
	void		installedChanged(	bool installed);
	void		loadingChanged(		bool loading);
	void		installingChanged(	bool installing);
	void		initializedChanged(	bool initialized);
	void		bundledChanged(		bool isBundled);
	void		registerForLoading(				const std::string & moduleName);
	void		registerForInstalling(			const std::string & moduleName);
	void		registerForInstallingModPkg(	const std::string & moduleName);
	void		descriptionReloaded(Modules::DynamicModule * dynMod);
	void		importsRChanged();
	void		errorChanged(bool error);

private:
	QFileInfo			_moduleFolder;
	moduleStatus		_status				= moduleStatus::initializing;
	std::string			_name,
						_title,
						_icon,
						_author,
						_website,
						_license,
						_loadLog			= "",
						_installLog			= "",
						_maintainer,
						_descriptionTxt,
						_modulePackage		= "",
						_version;
	bool				_installing			= false,
						_installed			= false,
						_loaded				= false,
						_loading			= false,
						_isDeveloperMod		= false,
						_initialized		= false,
						_bundled			= false,
						_isCommon			= false;
	AnalysisEntries		_menuEntries;
	stringset			_importsR;
	const char		*	_modulePostFix		= "_module",
					*	_exposedPostFix		= "_exposed";
	Description		*	_description		= nullptr;
	Upgrades		*	_upgrades			= nullptr;

	static std::string			_developmentModuleName;
	static const std::string	_moduleDirPostfix;
};


}

#endif // DYNAMICMODULE_H
