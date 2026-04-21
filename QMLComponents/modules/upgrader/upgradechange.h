#ifndef UPGRADECHANGE_H
#define UPGRADECHANGE_H

#include <string>
#include <vector>
#include <json/json.h>
#include "enumutilities.h"
#include "upgradeDefinitions.h"

// This file contains outdated upgrading options. In the future we should move towards doing everything through Upgrades.qml

namespace Modules {

DECLARE_ENUM(BoolOpType, AND, OR, NOT, XOR);
DECLARE_ENUM(ModifyType, Flatten); //Looks silly now but maybe we will have more then one possibility later ;)

extern const char * logId;

struct BoolOp
{
	BoolOp() : _noOp(true) {}
	BoolOp(Json::Value def);

	static bool optionContainsConditional(const Json::Value & conditional, const Json::Value & options);

	bool apply(const Json::Value & options) const;

	std::string toString() const;

private:
	BoolOpType					_type;
	std::vector<BoolOp>			_funcArgs;
	std::vector<Json::Value>	_valArgs;
	bool						_noOp = false;
};

/// Part of the monolithic upgrade process as defined by upgrades.json
class UpgradeChange
{
public:
	UpgradeChange(const Json::Value & upgradeStep);

	void applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const;

private:
	void applyRename(	Json::Value & options, const std::string & oldName, const std::string & newName,	UpgradeMsgs & msgs)	const;
	void applyCopy(		Json::Value & options, const std::string & oldName, const std::string & newName,	UpgradeMsgs & msgs)	const;
	void applyRemove(	Json::Value & options, const std::string & name,									UpgradeMsgs & msgs)	const;
	void applySetValue(	Json::Value & options, const std::string & name,	const Json::Value & newValue,	UpgradeMsgs & msgs) const;
	void applySetBool(	Json::Value & options, const std::string & name,	const BoolOp	  & newBool ,	UpgradeMsgs & msgs) const;
	void applyModifier(	Json::Value & options, const std::string & name,	const ModifyType	modifier,	UpgradeMsgs & msgs) const;

	BoolOp								_conditional;
	std::map<std::string, std::string>	_optionMsgs,
										_optionsRenamed,
										_optionsCopied;
	std::map<std::string, Json::Value>	_optionsNewValue;
	std::map<std::string, BoolOp>		_optionsBoolOp;
	std::set<std::string>				_optionsRemoved;
	std::map<std::string, ModifyType>	_optionsModified;

};

}

#endif // UPGRADECHANGE_H
