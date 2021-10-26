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
};

}

#endif // UPGRADES_H
