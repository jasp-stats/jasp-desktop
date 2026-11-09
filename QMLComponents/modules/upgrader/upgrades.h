#ifndef UPGRADES_H
#define UPGRADES_H

#include <QQuickItem>
#include "version.h"
#include "upgradeDefinitions.h"

namespace Modules
{

class Upgrade;

/// Part of the modulespecific upgrade process as defined in each (inst/)Upgrades.qml
/// It collects a group of Upgrade instances for a module
class Upgrades : public QQuickItem
{
	Q_OBJECT
	Q_PROPERTY(QString module READ module NOTIFY moduleChanged) //Readonly in QML
	
	friend Upgrade;

	typedef std::map<std::string,	Upgrade*>			UpgradePerFunction;
	typedef std::map<Version,		UpgradePerFunction>	FunctionsPerVersion;

public:
	Upgrades();
	
	QString module() const	{	return _module;	}
	
	bool	findClosestVersion(	const std::string & function,		Version & version);
	bool	hasUpgradesToApply(	const std::string & function, const Version	& version)  { Version v(version); return findClosestVersion(function, v); }
	void	applyUpgrade(		const std::string & function, const Version	& version, Json::Value & analysesJson, UpgradeMsgs & msgs, StepsTaken & stepsTaken);

	///Upgrade and Change* register themselves while Qt is still creating the QML items, an exception thrown from there cannot pass through Qt and terminates JASP.
	///So they store the error here instead and DynamicModule::loadUpgradesQML throws it once the QML has been created.
	static void							addLoadError(const std::string & error)	{ _loadErrors.push_back(error); }
	static std::vector<std::string>		takeLoadErrors();
	
	
public slots:
	void setModule(QString module);
	
signals:
	void moduleChanged(QString module);
	
private:
	void	addStep(	Upgrade * step);
	void	removeStep(	Upgrade * step);

private:
	FunctionsPerVersion	_steps;
	QString				_module; //To get the name of the module easily for error etc.

	static std::vector<std::string>	_loadErrors;
};

}

#endif // UPGRADES_H
