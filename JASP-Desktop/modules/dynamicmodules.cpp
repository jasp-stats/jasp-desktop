#include "dynamicmodules.h"

void DynamicModules::loadModule(QString moduleDir)
{
	Modules::DynamicModule		*newMod	= new Modules::DynamicModule(moduleDir, this);
	std::string					modName = newMod->name();

	if(_modules.count(modName) > 0)
	{
		delete _modules[modName];
		_modulesInstallNeeded.erase(modName);
	}

	_modules[modName] = newMod;

	if(newMod->installNeeded())			_modulesInstallNeeded.insert(modName);
	else if(newMod->loadingNeeded())	_modulesToBeLoaded.insert(modName);
	else								throw std::runtime_error("A module("+ newMod->name() +") that does not require installing or loading was \"loaded\" into JASP, this can't be right.");
}

Modules::DynamicModule* DynamicModules::requestModuleForSomethingAndRemoveIt(std::set<std::string> & theSet)
{
	if(theSet.size() == 0)
		return NULL;

	std::string installMe = *theSet.begin();
	theSet.erase(installMe);

	return _modules[installMe];
}

void DynamicModules::installationFailed(std::string moduleName, std::string errorMessage)
{
	std::cout << "Installing packages for module (" << moduleName << ") failed because of: " << errorMessage << std::endl;
	_modules[moduleName]->setInstalled(false);

}

void DynamicModules::installationSucceeded(std::string moduleName)
{
	std::cout << "Installing packages for module (" << moduleName<< ") succeeded!" << std::endl;
	_modules[moduleName]->setInstalled(true);

	if(_modules[moduleName]->loadingNeeded())	_modulesToBeLoaded.insert(moduleName);
	else										throw std::runtime_error("Unexpected! Module "+moduleName+" was just installed but doesn't want to be loaded..");
}


void DynamicModules::loadingFailed(std::string moduleName, std::string errorMessage)
{
	std::cout << "Loading packages for module (" << moduleName << ") failed because of: " << errorMessage << std::endl;
	_modules[moduleName]->setLoaded(false);
}

void DynamicModules::loadingSucceeded(std::string moduleName)
{
	std::cout << "Loading packages for module (" << moduleName<< ") succeeded!" << std::endl;
	_modules[moduleName]->setLoaded(true);
}
