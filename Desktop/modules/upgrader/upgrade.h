#ifndef UPGRADE_H
#define UPGRADE_H

#include <QQuickItem>
#include "version.h"
#include "upgradeDefinitions.h"
#include <unordered_set>

namespace Modules
{

class Upgrades;
class ChangeBase;


///Part of the modulespecific upgrade process as defined in each (inst/)Upgrades.qml
/// One instance of this describes an update from a particular version of its parent module to another particular version of it.
/// Switching between modules is impossible by design. Mostly because it would be confusing and I (Joris) think it is ok to have some restrictions here.
class Upgrade : public QQuickItem
{
	Q_OBJECT
	Q_PROPERTY(QString fromVersion		READ fromVersionQ		WRITE setFromVersionQ		NOTIFY fromVersionChanged		)
	Q_PROPERTY(QString toVersion		READ toVersionQ			WRITE setToVersionQ			NOTIFY toVersionChanged			)
	Q_PROPERTY(QString functionName		READ functionName		WRITE setFunctionName		NOTIFY functionNameChanged		) //Aka fromFunction or just function if it doesnt need to change
	Q_PROPERTY(QString newFunctionName	READ newFunctionName	WRITE setNewFunctionName	NOTIFY newFunctionNameChanged	) //This property can be set if the functionname changed in toVersion. "" here means undefined
	Q_PROPERTY(QString msg READ msg WRITE setMsg NOTIFY msgChanged)


	friend ChangeBase;
public:
	Upgrade();
	~Upgrade();

	void applyUpgrade(const std::string & function, const Version & version, Json::Value & analysesJson, UpgradeMsgs & msgs, StepsTaken & stepsTaken);

	QString toString();
	
	QString newFunctionName()	const { return _newFunctionName != "" ? _newFunctionName : _functionName;	}
	QString functionName()		const { return _functionName;		}
	QString fromVersionQ()		const { return _fromVersion;		}
	Version fromVersion()		const;
	QString toVersionQ()		const { return _toVersion;			}
	Version toVersion()			const;
	QString module()			const;
	bool	isModuleDev()		const;

	const QString &msg() const;
	void setMsg(const QString &newMsg);

public slots:
	void setFromVersionQ(		QString fromVersion);
	void setToVersionQ(			QString toVersion);
	void setFunctionName(		QString functionName);
	void setNewFunctionName(	QString newFunctionName);

signals:
	void fromVersionChanged();
	void toVersionChanged();
	void functionNameChanged(	QString functionName);
	void newFunctionNameChanged(QString newFunctionName);

	void msgChanged();

private slots:
	void registerStep(		QQuickItem * parent);

private:
	void	addChange(		ChangeBase * change);
	void	removeChange(	ChangeBase * change);

private:
	Upgrades				*	_upgrades		= nullptr;
	QString						_fromVersion,
								_toVersion,
								_functionName,
								_newFunctionName,
								_msg;
	std::vector<ChangeBase*>	_changes;
};

}

#endif // UPGRADE_H
