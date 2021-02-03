#include "utilities/qutils.h"

#define ENUM_DECLARATION_CPP
#include "upgradestep.h"



namespace Modules
{

const char			*	_boolOp		= "_boolOp",
					*	_args		= "_args",
					*	_renamed	= "_renamed",
					*	_copied		= "_copied",
					*	_modified	= "_modified";




BoolOp::BoolOp(Json::Value def)
{
	if(def.isNull())
		_noOp = true;
	else if(def.isObject() && !def.isMember(_boolOp) && !def.isMember(_args))
	{
		_type = BoolOpType::AND;

		_valArgs.push_back(def); //If args wasn't defined then just assume that the whole thing is an arg...
	}
	else if(def.isObject() && def.isMember(_args))
	{
		_type = BoolOpTypeFromQString(tq(def.get(_boolOp, "AND").asString()).toUpper());

		if(!def[_args].isArray())
			throw upgradeLoadError(def, "Loading boolean conditional failed because \"args\" was not an array...");

		for(const Json::Value & arg : def[_args])
			if(arg.isObject())
			{
				if(arg.isMember(_boolOp) || arg.isMember(_args))	_funcArgs.push_back(BoolOp(arg));
				else												_valArgs.push_back(arg);
			}
			else
				throw upgradeLoadError(arg, "Could not parse arguments to boolean conditional!");

		if(_type == BoolOpType::NOT && _funcArgs.size() + _valArgs.size() != 1)
			throw upgradeLoadError(def, "A boolean conditional with NOT was defined, but it was given more than one argument, this is not allowed!");
	}
	else
		throw upgradeLoadError(def, "Could not parse boolean conditional... Make sure it is either a set of options or something like: { \"boolOp\":\"AND\", \"args\":[{\"someOption\":true}]}");
}

bool BoolOp::optionContainsConditional(const Json::Value & conditional, const Json::Value & options)
{
	for(const std::string & option : conditional.getMemberNames())
		if(!options.isMember(option))
			//throw upgradeError("There was an error updating because a boolean operator in a conditional depends on a non-existent option.\nThe option is: " + option + " and the boolean operator: '" + conditional.toStyledString() + "'");
			return false; //Actually makes more sense to just return false

	for(const std::string & option : conditional.getMemberNames())
		if(conditional[option] != options[option])
			return false;

	return true;
}

std::string BoolOp::toString() const
{
	if(_noOp) return "";

	std::stringstream str;

	size_t totalArgs = _funcArgs.size() + _valArgs.size();

	if(totalArgs > 1) str << "( ";


	auto makeArgs = [&](const std::string & op)
	{
		bool firstArg = true;

		for(const BoolOp & funcArg : _funcArgs)
		{
			str << funcArg.toString();
			if(!firstArg)
				str << " " << op << " ";
			firstArg = false;
		}

		for(const Json::Value & valArg : _valArgs)
		{
			str << valArg;
			if(!firstArg)
				str << " " << op << " ";
			firstArg = false;
		}
	};

	switch(_type)
	{
	case BoolOpType::NOT:
		str << "!" ;
		if(_funcArgs.size() == 1)	str << _funcArgs[0].toString();
		else						str << _valArgs[0];
		break;

	case BoolOpType::AND:		makeArgs("&&");		break;
	case BoolOpType::OR:		makeArgs("||");		break;
	case BoolOpType::XOR:		makeArgs("^^");		break;
	}

	if(totalArgs > 1) str << " )";

	return str.str();
}

bool BoolOp::apply(const Json::Value & options) const
{
	if(_noOp)
		return true;

	switch(_type)
	{
	case BoolOpType::NOT:
		if(_funcArgs.size() == 1)	return !_funcArgs[0].apply(options);
		else						return !optionContainsConditional(_valArgs[0], options);

	case BoolOpType::AND:
		for(const BoolOp & func : _funcArgs)
			if(!func.apply(options))
				return false;

		for(const Json::Value & val : _valArgs)
			if(!optionContainsConditional(val, options))
				return false;

		return true;

	case BoolOpType::OR:
		for(const BoolOp & func : _funcArgs)
			if(func.apply(options))
				return true;

		for(const Json::Value & val : _valArgs)
			if(!optionContainsConditional(val, options))
				return true;

		return false;

	case BoolOpType::XOR:
	{
		bool foundSomethingAlready = false;
		for(const BoolOp & func : _funcArgs)
			if(func.apply(options))
			{
				if(foundSomethingAlready)
					return false;
				foundSomethingAlready = true;
			}

		for(const Json::Value & val : _valArgs)
			if(optionContainsConditional(val, options))
			{
				if(foundSomethingAlready)
					return false;
				foundSomethingAlready = true;
			}

		return foundSomethingAlready;
	}
	}

	throw std::runtime_error("It should really be quite impossible to reach this point at all but gcc does not get that, so here we are.");
}

UpgradeChange::UpgradeChange(const Json::Value & upgradeStep)
	: _conditional(upgradeStep.get("conditional", Json::nullValue))
{
	const Json::Value & changes = upgradeStep["changes"];

	if(!changes.isObject())
		throw upgradeLoadError(changes, "Loading an upgrade step failed because the changes were not a json object, it should be. Each name is the name of the option that will be added, set to a certain value or removed.");


	for(const std::string & option : changes.getMemberNames())
		if		(changes[option].isNull())		_optionsRemoved.insert(option);
		else if	(!changes[option].isObject())	_optionsNewValue[option] = changes[option];
		else
		{
			const Json::Value & change = changes[option];

			if(change.isMember(_renamed))		_optionsRenamed [option] = change[_renamed].asString();
			else if(change.isMember(_copied))	_optionsCopied	[option] = change[_copied].asString();
			else if(change.isMember(_modified))	_optionsModified[option] = ModifyTypeFromString(change[_modified].asString()); //Maybe check for error?
			else if(change.isMember(_args) ||
					change.isMember(_boolOp))	_optionsBoolOp	[option] = BoolOp(change);
			else								_optionsNewValue[option] = change;
		}

	const Json::Value msgs = upgradeStep.get("msgs", Json::objectValue);

	for(const std::string & option : msgs.getMemberNames())
		_optionMsgs[option] = msgs[option].asString();
}


void UpgradeChange::applyRename(Json::Value & options, const std::string & oldName, const std::string & newName, UpgradeMsgs & msgs) const
{
	if(options.isMember(newName))
		throw upgradeError("Could not rename option '" + oldName + "' to '" + newName + "' because options already contains '" + newName + "'");

	if(!options.isMember(oldName))
		throw upgradeError("Could not rename option '" + oldName + "' to '" + newName + "' because options does not contain '" + oldName + "'");

	options[newName] = options[oldName];
	options.removeMember(oldName);

	msgs[newName] = msgs[oldName];
	msgs.erase(oldName);

	msgs[logId].push_back(prefixLog + "Renamed option '" + oldName + "' to '" + newName + "'");
}

void UpgradeChange::applyCopy(Json::Value & options, const std::string & oldName, const std::string & newName, UpgradeMsgs & msgs) const
{
	//It is ok to overwrite whatever is in newName? Because it is a sort of "set value"

	if(!options.isMember(oldName))
		throw upgradeError("Could not copy option '" + oldName + "' to '" + newName + "' because options does not contain '" + oldName + "'");

	options[newName]	= options[oldName];
	msgs[newName]		= msgs[oldName];

	msgs[logId].push_back(prefixLog + "Copied option '" + oldName + "' to '" + newName + "'");
}

void UpgradeChange::applyRemove(Json::Value & options, const std::string & name, UpgradeMsgs & msgs) const
{
	if(!options.isMember(name))
		throw upgradeError("Could not erase option '" + name + "' because options does not contain it.");

	options.removeMember(name);
	msgs.erase(name);

	msgs[logId].push_back(prefixLog + "Removed option '" + name + "'");
}

void UpgradeChange::applySetValue(Json::Value & options, const std::string & name,	const Json::Value & newValue, UpgradeMsgs & msgs) const
{
	options[name] = newValue;

	msgs[logId].push_back(prefixLog + "Set option '" + name + "' to '" + newValue.toStyledString() + "'");

}

void UpgradeChange::applySetBool(Json::Value & options, const std::string & name,	const BoolOp & newBool, UpgradeMsgs & msgs) const
{
	options[name] = newBool.apply(options);

	msgs[logId].push_back(prefixLog + "Set option '" + name + "' to '" + (options[name].asBool() ? "true" : "false") + "'");
}

void UpgradeChange::applyUpgrade(Json::Value & options, UpgradeMsgs & msgs) const
{
	if(!_conditional.apply(options))
		return;

	const std::string cond = _conditional.toString();
	if(cond != "")
		msgs[logId].push_back("Conditional passed:\n" + cond + "=============================================");

	for(const auto & newOldName		: _optionsCopied	) applyCopy(	options, newOldName.second,		newOldName.first,		msgs);
	for(const auto & newOldName		: _optionsRenamed	) applyRename(	options, newOldName.second,		newOldName.first,		msgs);
	for(const auto & removeMe		: _optionsRemoved	) applyRemove(	options, removeMe,										msgs);
	for(const auto & nameValue		: _optionsNewValue	) applySetValue(options, nameValue.first,		nameValue.second,		msgs);
	for(const auto & nameValue		: _optionsBoolOp	) applySetBool(	options, nameValue.first,		nameValue.second,		msgs);
	for(const auto & optionModifier : _optionsModified	) applyModifier(options, optionModifier.first,	optionModifier.second,	msgs);

	for(const auto & optionMsg : _optionMsgs)
		msgs[optionMsg.first].push_back(optionMsg.second);
}

void UpgradeChange::applyModifier(Json::Value & options, const std::string & name,	const ModifyType modifier, UpgradeMsgs & msgs) const
{
	if(!options.isMember(name))
		throw upgradeError("Could not modify option '" + name + "' with operation '"+ ModifyTypeToString(modifier) +"' because options does not contain it.");

	switch(modifier)
	{
	case ModifyType::Flatten:
	{
		Json::Value outList;

		std::function<void(const Json::Value & in)> flatFunc = [&](const Json::Value & in) {
			if(in.isArray())
				for(const Json::Value & entry : in)
					flatFunc(entry);
			else if(in.isObject())
				for(const std::string & memberName : in.getMemberNames())
					flatFunc(in[memberName]);
			else
				outList.append(in);
		};

		flatFunc(options[name]);
		options[name] = outList;

		break;
	}
	default: throw std::runtime_error("Upgrading with a modifier '" + ModifyTypeToString(modifier) + "' is not yet supported by JASP!");
	}

	msgs[logId].push_back(prefixLog + "Modified option '" + name + "' with modifier '" + modifier + "'");
}

}
