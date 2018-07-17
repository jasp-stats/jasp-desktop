#ifndef DYNAMICMODULES_H
#define DYNAMICMODULES_H

#include <map>
#include <set>
#include <QObject>
#include "dynamicmodule.h"

class DynamicModules : public QObject
{
	Q_OBJECT
public:
	explicit DynamicModules(QObject *parent) : QObject(parent) {}

	void		loadModule(QString moduleDir);

	bool		aModuleNeedsToBeLoadedInR()					{ return _modulesToBeLoaded.size() > 0; }
	bool		aModuleNeedsPackagesInstalled()				{ return _modulesInstallNeeded.size() > 0; }
	Json::Value	requestJsonForPackageLoadingRequest()		{ return requestModuleForSomethingAndRemoveIt(_modulesToBeLoaded)->requestJsonForPackageLoadingRequest(); }
	Json::Value	requestJsonForPackageInstallationRequest()	{ return requestModuleForSomethingAndRemoveIt(_modulesInstallNeeded)->requestJsonForPackageInstallationRequest(); }

private:
	Modules::DynamicModule* requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet);


public slots:
	void installationSucceeded(	std::string moduleName);
	void installationFailed(	std::string moduleName, std::string errorMessage);
	void loadingSucceeded(		std::string moduleName);
	void loadingFailed(			std::string moduleName, std::string errorMessage);

private:
	std::map<std::string, Modules::DynamicModule*>	_modules;
	std::set<std::string>							_modulesInstallNeeded,
													_modulesToBeLoaded;

};

#endif // DYNAMICMODULES_H
