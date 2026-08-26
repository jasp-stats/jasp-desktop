#include "scriptnode.h"
#include <cmath>
#include <cstdio>
#include <cstdlib>

static std::string numberToRString(double v)
{
	if(std::isnan(v)) return "NaN";
	if(std::isinf(v)) return v > 0 ? "Inf" : "-Inf";

	if(v == std::floor(v) && std::abs(v) < 1e15)
		return std::to_string(static_cast<long long>(v));

	for(int prec = 1; prec <= 17; prec++)
	{
		char buf[64];
		snprintf(buf, sizeof(buf), "%.*g", prec, v);
		if(std::strtod(buf, nullptr) == v)
			return buf;
	}

	char buf[64];
	snprintf(buf, sizeof(buf), "%.17g", v);
	return buf;
}

ScriptNode::ScriptNode(ScriptNode * parent)
	: QObject(nullptr)
	, _parent(parent)
{
}

void ScriptNode::deleteTree(ScriptNode * node)
{
	if(!node) return;

	for(int i = 0; i < node->childCount(); i++)
		deleteTree(node->childAt(i));

	delete node;
}

std::string ScriptNode::nodeTypeString() const
{
	switch(type())
	{
	case Type::Operator:			return "Operator";
	case Type::OperatorVertical:	return "OperatorVertical";
	case Type::Function:			return "Function";
	case Type::RowFunction:			return "RowFunction";
	case Type::Column:				return "Column";
	case Type::Number:				return "Number";
	case Type::Boolean:				return "Boolean";
	case Type::String:				return "String";
	}
	return "";
}

ScriptNode::Type ScriptNode::typeFromString(const std::string & str)
{
	if(str == "Operator")			return Type::Operator;
	if(str == "OperatorVertical")	return Type::OperatorVertical;
	if(str == "Function")			return Type::Function;
	if(str == "RowFunction")		return Type::RowFunction;
	if(str == "Column")				return Type::Column;
	if(str == "Number")				return Type::Number;
	if(str == "Boolean")			return Type::Boolean;
	if(str == "String")				return Type::String;

	throw std::runtime_error("Unknown script node type: " + str);
}

ScriptNode * ScriptNode::fromJson(const Json::Value & json, ScriptNode * parent)
{
	if(json.isNull() || !json.isObject())
		return nullptr;

	const std::string nodeType = json.get("nodeType", "").asString();
	const std::string toolTip  = json.get("toolTipText", "").asString();

	ScriptNode * result = nullptr;

	if(nodeType == "Operator" || nodeType == "OperatorVertical")
	{
		auto * op = new ScriptNodeOperator(json.get("operator", "+").asString(), nodeType == "OperatorVertical", parent);

		op->setLeft(	fromJson(json.get("leftArgument", Json::nullValue),		op));
		op->setRight(	fromJson(json.get("rightArgument", Json::nullValue),	op));

		result = op;
	}
	else if(nodeType == "Function")
	{
		std::vector<ScriptNodeFunction::Argument> args;
		const Json::Value & argsJson = json.get("arguments", Json::arrayValue);

		for(Json::ArrayIndex i = 0; i < argsJson.size(); i++)
		{
			const Json::Value & argJson = argsJson[i];

			ScriptNodeFunction::Argument arg;
			arg.name		= argJson.get("name", "").asString();
			arg.optional	= !arg.name.empty() && arg.name[0] == '?';

			if(arg.optional)
				arg.name = arg.name.substr(1);

			const Json::Value & keysJson = argJson.get("dropKeys", Json::arrayValue);
			for(Json::ArrayIndex k = 0; k < keysJson.size(); k++)
				arg.dropKeys.push_back(keysJson[k].asString());

			args.push_back(arg);
		}

		auto * func = new ScriptNodeFunction(json.get("functionName", "").asString(), args, parent);

		for(Json::ArrayIndex i = 0; i < argsJson.size(); i++)
			func->setArgumentValue(static_cast<int>(i), fromJson(argsJson[i].get("argument", Json::nullValue), func));

		result = func;
	}
	else if(nodeType == "RowFunction")
	{
		auto * rowFunc = new ScriptNodeRowFunction(json.get("functionName", "").asString(), parent);

		const Json::Value & droppedItems = json.get("droppedItems", Json::arrayValue);
		for(Json::ArrayIndex i = 0; i < droppedItems.size(); i++)
		{
			const std::string itemStr = droppedItems[i].asString();

			if(itemStr == "null" || itemStr.empty())
				rowFunc->addChild(nullptr);
			else
			{
				Json::Value itemJson;
				Json::Reader().parse(itemStr, itemJson);
				rowFunc->addChild(fromJson(itemJson, rowFunc));
			}
		}

		if(rowFunc->childCount() == 0)
			rowFunc->addChild(nullptr);

		result = rowFunc;
	}
	else if(nodeType == "Column")
	{
		result = new ScriptNodeColumn(
			json.get("columnName", "").asString(),
			json.get("columnTypeUser", -1).asInt(),
			json.get("columnTypeDrop", -1).asInt(),
			parent);

		if(json.isMember("dataSetName"))
			static_cast<ScriptNodeColumn*>(result)->setDataSetName(json["dataSetName"].asString());
	}
	else if(nodeType == "Number")
	{
		auto * lit = new ScriptNodeLiteral(Type::Number, parent);
		lit->setNumberValue(json.get("value", 0).asDouble());
		result = lit;
	}
	else if(nodeType == "Boolean")
	{
		auto * lit = new ScriptNodeLiteral(Type::Boolean, parent);

		const Json::Value & val = json.get("value", false);
		if(val.isString())	lit->setBoolValue(val.asString() == "TRUE");
		else				lit->setBoolValue(val.asBool());

		result = lit;
	}
	else if(nodeType == "String")
	{
		auto * lit = new ScriptNodeLiteral(Type::String, parent);
		lit->setStringValue(json.get("text", "").asString());
		result = lit;
	}

	if(result && !toolTip.empty())
		result->setToolTip(toolTip);

	return result;
}

// --- ScriptNodeOperator ---

ScriptNodeOperator::ScriptNodeOperator(const std::string & op, bool vertical, ScriptNode * parent)
	: ScriptNode(parent)
	, _op(op)
	, _vertical(vertical)
{
}

void ScriptNodeOperator::setLeft(ScriptNode * node)
{
	if(node) node->setParent(this);
	_left = node;
}

void ScriptNodeOperator::setRight(ScriptNode * node)
{
	if(node) node->setParent(this);
	_right = node;
}

Json::Value ScriptNodeOperator::toJson() const
{
	Json::Value json;
	json["nodeType"]		= nodeTypeString();
	json["operator"]		= _op;
	json["leftArgument"]	= _left ? _left->toJson() : Json::nullValue;
	json["rightArgument"]	= _right ? _right->toJson() : Json::nullValue;

	if(!_toolTip.empty())
		json["toolTipText"] = _toolTip;

	return json;
}

std::string ScriptNodeOperator::toR(const ScriptColumnTypeProvider * typeProvider) const
{
	std::string out = "(";
	out += _left ? _left->toR(typeProvider) : "null";
	out += " " + _op + " ";
	out += _right ? _right->toR(typeProvider) : "null";
	out += ")";
	return out;
}

stringvec ScriptNodeOperator::dragKeys() const
{
	const ScriptOperatorDef * def = ScriptConstructorRegistry::instance().operatorDef(_op);

	if(!def) return {"number"};

	return def->dragKeys(ScriptConstructorMode::Filter);
}

bool ScriptNodeOperator::isComplete() const
{
	bool leftOk		= _left ? _left->isComplete() : false;
	bool rightOk	= _right ? _right->isComplete() : false;

	return leftOk && rightOk;
}

stringvec ScriptNodeOperator::dropKeysLeft() const
{
	const ScriptOperatorDef * def = ScriptConstructorRegistry::instance().operatorDef(_op);
	return def ? def->dropKeysLeft(ScriptConstructorMode::Filter) : stringvec{"number"};
}

stringvec ScriptNodeOperator::dropKeysRight() const
{
	const ScriptOperatorDef * def = ScriptConstructorRegistry::instance().operatorDef(_op);
	return def ? def->dropKeysRight(ScriptConstructorMode::Filter) : stringvec{"number"};
}

// --- ScriptNodeFunction ---

ScriptNodeFunction::ScriptNodeFunction(const std::string & functionName, ScriptNode * parent)
	: ScriptNode(parent)
	, _functionName(functionName)
{
	const ScriptFunctionDef * def = ScriptConstructorRegistry::instance().functionDef(functionName);

	if(def)
		for(const ScriptParamDef & param : def->params)
			_arguments.push_back({param.name, param.dropKeys, param.optional, nullptr});
}

ScriptNodeFunction::ScriptNodeFunction(const std::string & functionName, const std::vector<Argument> & args, ScriptNode * parent)
	: ScriptNode(parent)
	, _functionName(functionName)
	, _arguments(args)
{
}

void ScriptNodeFunction::addArgument(const Argument & arg)
{
	_arguments.push_back(arg);
}

void ScriptNodeFunction::setArgumentValue(int index, ScriptNode * node)
{
	if(index < 0 || index >= static_cast<int>(_arguments.size()))
		return;

	if(node) node->setParent(this);
	_arguments[index].value = node;
}

int ScriptNodeFunction::argumentIndex(const std::string & name) const
{
	for(int i = 0; i < static_cast<int>(_arguments.size()); i++)
		if(_arguments[i].name == name)
			return i;

	return -1;
}

Json::Value ScriptNodeFunction::toJson() const
{
	Json::Value json;
	json["nodeType"]		= "Function";
	json["functionName"]	= _functionName;
	json["arguments"]		= Json::arrayValue;

	for(const Argument & arg : _arguments)
	{
		Json::Value argJson;
		argJson["name"]		= arg.optional ? "?" + arg.name : arg.name;
		argJson["dropKeys"]	= Json::arrayValue;
		for(const std::string & key : arg.dropKeys)
			argJson["dropKeys"].append(key);
		argJson["argument"]	= arg.value ? arg.value->toJson() : Json::nullValue;

		json["arguments"].append(argJson);
	}

	if(!_toolTip.empty())
		json["toolTipText"] = _toolTip;

	return json;
}

std::string ScriptNodeFunction::toR(const ScriptColumnTypeProvider * typeProvider) const
{
	std::string out = _functionName + "(";

	for(int i = 0; i < static_cast<int>(_arguments.size()); i++)
	{
		if(i > 0) out += ", ";
		out += _arguments[i].value ? _arguments[i].value->toR(typeProvider) : "NULL";
	}

	const ScriptFunctionDef * def = ScriptConstructorRegistry::instance().functionDef(_functionName);
	if(def && def->addsNaRm())
		out += ", na.rm=TRUE";

	out += ")";
	return out;
}

stringvec ScriptNodeFunction::dragKeys() const
{
	const ScriptFunctionDef * def = ScriptConstructorRegistry::instance().functionDef(_functionName);
	return def ? def->dragKeys() : stringvec{"number"};
}

bool ScriptNodeFunction::isComplete() const
{
	for(const Argument & arg : _arguments)
	{
		if(arg.optional)
			continue;

		if(!arg.value || !arg.value->isComplete())
			return false;
	}

	return true;
}

// --- ScriptNodeRowFunction ---

ScriptNodeRowFunction::ScriptNodeRowFunction(const std::string & functionName, ScriptNode * parent)
	: ScriptNode(parent)
	, _functionName(functionName)
{
}

void ScriptNodeRowFunction::setChild(int index, ScriptNode * node)
{
	if(index < 0 || index >= static_cast<int>(_children.size()))
		return;

	if(node) node->setParent(this);
	_children[index] = node;
}

void ScriptNodeRowFunction::addChild(ScriptNode * node)
{
	if(node) node->setParent(this);
	_children.push_back(node);
}

void ScriptNodeRowFunction::removeChildAt(int index)
{
	if(index < 0 || index >= static_cast<int>(_children.size()))
		return;

	_children.erase(_children.begin() + index);
}

int ScriptNodeRowFunction::childCountFilled() const
{
	int count = 0;
	for(ScriptNode * child : _children)
		if(child) count++;
	return count;
}

void ScriptNodeRowFunction::ensureTrailingEmptySlot()
{
	// Keep a free (null) child at the end so the user can always drop another column.
	for(ScriptNode * child : _children)
		if(!child)
			return;
	_children.push_back(nullptr);
}

Json::Value ScriptNodeRowFunction::toJson() const
{
	Json::Value json;
	json["nodeType"]		= "RowFunction";
	json["functionName"]	= _functionName;
	json["droppedItems"]	= Json::arrayValue;

	for(ScriptNode * child : _children)
	{
		if(child)
		{
			Json::Value childJson = child->toJson();
			Json::StreamWriterBuilder builder;
			builder["indentation"] = "";
			json["droppedItems"].append(Json::writeString(builder, childJson));
		}
		else
			json["droppedItems"].append("null");
	}

	if(!_toolTip.empty())
		json["toolTipText"] = _toolTip;

	return json;
}

std::string ScriptNodeRowFunction::toR(const ScriptColumnTypeProvider * typeProvider) const
{
	std::string out = _functionName + "NaRm(";

	bool first = true;
	for(ScriptNode * child : _children)
	{
		if(!child) continue;

		if(!first) out += ", ";
		out += child->toR(typeProvider);
		first = false;
	}

	out += ")";
	return out;
}

stringvec ScriptNodeRowFunction::dragKeys() const
{
	return {"number"};
}

bool ScriptNodeRowFunction::isComplete() const
{
	for(ScriptNode * child : _children)
		if(child && child->isComplete())
			return true;

	return false;
}

// --- ScriptNodeColumn ---

ScriptNodeColumn::ScriptNodeColumn(const std::string & columnName, int columnTypeUser, int columnTypeDrop, ScriptNode * parent)
	: ScriptNode(parent)
	, _columnName(columnName)
	, _columnTypeUser(columnTypeUser)
	, _columnTypeDrop(columnTypeDrop)
{
}

void ScriptNodeColumn::setColumnTypeUser(int t)
{
	_columnTypeUser = t;
}

void ScriptNodeColumn::setColumnTypeDrop(int t)
{
	_columnTypeDrop = t;
}

int ScriptNodeColumn::effectiveColumnType(int actualColumnType) const
{
	if(_columnTypeDrop != -1)	return _columnTypeDrop;
	if(_columnTypeUser != -1)	return _columnTypeUser;
	return actualColumnType;
}

Json::Value ScriptNodeColumn::toJson() const
{
	Json::Value json;
	json["nodeType"]			= "Column";
	json["columnName"]			= _columnName;
	json["columnTypeUser"]		= _columnTypeUser;
	json["columnTypeDrop"]		= _columnTypeDrop;

	if(!_dataSetName.empty())
		json["dataSetName"] = _dataSetName;

	if(!_toolTip.empty())
		json["toolTipText"] = _toolTip;

	return json;
}

std::string ScriptNodeColumn::toR(const ScriptColumnTypeProvider * typeProvider) const
{
	int actualType = typeProvider ? typeProvider->columnType(_columnName) : 1;
	int effective = effectiveColumnType(actualType);

	return _columnName + "." + ScriptConstructorRegistry::columnTypeString(effective);
}

stringvec ScriptNodeColumn::dragKeys() const
{
	if(_columnTypeDrop != -1)
		return ScriptConstructorRegistry::dropKeysForColumnType(_columnTypeDrop);

	return {"number", "string", "ordered"};
}

// --- ScriptNodeLiteral ---

ScriptNodeLiteral::ScriptNodeLiteral(Type literalType, ScriptNode * parent)
	: ScriptNode(parent)
	, _literalType(literalType)
{
}

Json::Value ScriptNodeLiteral::toJson() const
{
	Json::Value json;
	json["nodeType"] = nodeTypeString();

	switch(_literalType)
	{
	case Type::Number:	json["value"] = _numberValue;	break;
	case Type::Boolean:	json["value"] = _boolValue ? "TRUE" : "FALSE";	break;
	case Type::String:	json["text"] = _stringValue;	break;
	default: break;
	}

	if(!_toolTip.empty())
		json["toolTipText"] = _toolTip;

	return json;
}

std::string ScriptNodeLiteral::toR(const ScriptColumnTypeProvider *) const
{
	switch(_literalType)
	{
	case Type::Number:	return numberToRString(_numberValue);
	case Type::Boolean:	return _boolValue ? "TRUE" : "FALSE";
	case Type::String:	return "'" + _stringValue + "'";
	default:			return "";
	}
}

stringvec ScriptNodeLiteral::dragKeys() const
{
	switch(_literalType)
	{
	case Type::Number:	return {"number"};
	case Type::Boolean:	return {"boolean"};
	case Type::String:	return {"string"};
	default:			return {};
	}
}
