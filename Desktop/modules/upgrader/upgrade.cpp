#include "gui/preferencesmodel.h"
#include "utilities/qutils.h"
#include "changebase.h"
#include "upgrades.h"
#include "upgrade.h"
#include "log.h"

namespace Modules
{

Upgrade::Upgrade()
{
	connect(this, &QQuickItem::parentChanged, this, &Upgrade::registerStep);
}

void Upgrade::applyUpgrade(const std::string & function, const Version & version, Json::Value & analysesJson, UpgradeMsgs & msgs, StepsTaken & stepsTaken)
{
	if(function != fq(functionName()))	throw upgradeError(fq(tr("Wrong Upgrade being applied, was looking for function '%1' but this upgrade is for: '%2'").arg(tq(function)).arg(functionName())));
	if(version	>  fromVersion())		throw upgradeError(fq(tr("Wrong Upgrade being applied, was looking for version '%1' but this upgrade is for: '%2' or lower").arg(tq(version.asString())).arg(tq(fromVersion().asString()))));
	
	StepTaken	fromStep(		{fq(module()),	fq(functionName()),		fromVersion()	}),
				aboutToStep(	{fq(module()),	fq(newFunctionName()),	toVersion()		});

	stepsTaken.insert(fromStep); //We want to remember where we come from

	if(stepsTaken.count(aboutToStep) > 0)
		throw upgradeError(fq(tr("Aborting upgrade because a loop was detected!\n\nIf %1 is taken, eventually it is reached again.\n\nThis should definitely not happen, perhaps the module author of '%2' can be of assistance").arg(toString()).arg(module())));
	
	analysesJson["name"]							= fq(newFunctionName());
	analysesJson["dynamicModule"]["analysisEntry"]	= fq(newFunctionName());
	analysesJson["dynamicModule"]["moduleVersion"]	= toVersion().asString();

	if(_msg.size())
		msgs[""].push_back(fq(_msg));

	//No loop so far, lets apply some changes for this step
	for(ChangeBase * change : _changes)
		try
		{
			Log::log() << "Checking if condition for change " << change->toString() << " is satisfied, ";
			if(change->conditionSatisfied(analysesJson["options"]))
			{
				Log::log(false) << "it is and applying the change!" << std::endl;
				change->applyUpgrade(analysesJson["options"], msgs);
			}
			else
				Log::log(false) << "it isn't and moving on." << std::endl;
		}
		catch(upgradeError & error)
		{
			//If we are not in developermode it would be better to keep going instead of crashing the whole upgradeprocess
			if(PreferencesModel::prefs()->developerMode())
				throw error;
			else
				Log::log() << "Change had a problem: " << error.what() << std::endl;
		}
}

QString Upgrade::toString() 
{ 
	return "Module '" + module() + "' from version '" + fromVersionQ() + "' to '" + toVersionQ() + "' for function '" + functionName() + "'" + ( newFunctionName() == functionName() ? "" : " being renamed to '" + newFunctionName() + "'");
}

Version Upgrade::fromVersion() const
{
	try
	{
		return Version(fq(_fromVersion));
	}
	catch(Version::encodingError & e)
	{
		throw upgradeError("Incorrect fromVersion: '" + fq(_fromVersion) + "' supplied to Upgrade...");
	}
}

Version Upgrade::toVersion() const
{
	try
	{
		return Version(fq(_toVersion));
	}
	catch(Version::encodingError & e)
	{
		throw upgradeError("Incorrect toVersion: '" + fq(_toVersion) + "' supplied to Upgrade...");
	}
}

QString Upgrade::module() const 
{
	return _upgrades ? _upgrades->module() : "???";
}


void Upgrade::registerStep(QQuickItem * parent)
{
	Upgrades * upgrades = qobject_cast<Upgrades*>(parent);
	
	if(!upgrades && parent)
		throw upgradeLoadError(fq(toString()), "An Upgrade Item must always be a child of Upgrades");

	if(upgrades != _upgrades)
	{
		if(_upgrades)
			_upgrades->removeStep(this);

		_upgrades = upgrades;

		if(_upgrades)
			_upgrades->addStep(this);
	}
}

Upgrade::~Upgrade()
{
	if(_upgrades)
		_upgrades->removeStep(this);
}

void Upgrade::addChange(ChangeBase *change) 
{ 
	for(ChangeBase * c : _changes) 
		if(c == change) return;
	
	_changes.push_back(change);		
}

void Upgrade::removeChange(ChangeBase *change)
{
	for(size_t i=_changes.size(); i>0; i--)
		if(_changes[i-1] == change)
			_changes.erase(_changes.begin() + i - 1);
}

void Upgrade::setFromVersionQ(QString fromVersion)
{
	if (_fromVersion == fromVersion)
		return;

	_fromVersion = fromVersion;
	emit fromVersionChanged();
}

void Upgrade::setToVersionQ(QString toVersion)
{
	if (_toVersion == toVersion)
		return;

	_toVersion = toVersion;
	emit toVersionChanged();
}

void Upgrade::setFunctionName(QString functionName)
{
	if (_functionName == functionName)
		return;

	_functionName = functionName;
	emit functionNameChanged(_functionName);
}

void Upgrade::setNewFunctionName(QString newFunctionName)
{
	if (_newFunctionName == newFunctionName)
		return;

	_newFunctionName = newFunctionName;
	emit newFunctionNameChanged(_newFunctionName);
}

const QString &Upgrade::msg() const
{
	return _msg;
}

void Upgrade::setMsg(const QString &newMsg)
{
	if (_msg == newMsg)
		return;
	_msg = newMsg;
	emit msgChanged();
}

}
