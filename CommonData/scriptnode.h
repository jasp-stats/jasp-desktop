#ifndef SCRIPTNODE_H
#define SCRIPTNODE_H

#include <json/json.h>
#include <string>
#include <vector>
#include <QObject>
#include "utils.h"
#include "scriptconstructorregistry.h"

class ScriptNodeModel;

/// Provides the actual column type for a column name (needed for R code generation of Column nodes).
/// The view/integration layer implements this using ColumnsModel or DataSet.
class ScriptColumnTypeProvider
{
public:
	virtual ~ScriptColumnTypeProvider() = default;
	virtual int columnType(const std::string & columnName) const = 0;
};

class ScriptNode : public QObject
{
	Q_OBJECT

public:
	enum class Type { Operator, OperatorVertical, Function, RowFunction, Column, Number, Boolean, String };

	explicit ScriptNode(ScriptNode * parent = nullptr);
	virtual ~ScriptNode() = default;

	virtual Type			type()		const = 0;
	virtual Json::Value		toJson()	const = 0;
	virtual std::string		toR(const ScriptColumnTypeProvider * typeProvider = nullptr) const = 0;
	virtual stringvec		dragKeys()	const = 0;
	virtual bool			isComplete() const = 0;

	virtual ScriptNode	*	leftChild()			const	{ return nullptr; }
	virtual ScriptNode	*	rightChild()		const	{ return nullptr; }
	virtual int				childCount()		const	{ return 0; }
	virtual ScriptNode	*	childAt(int)		const	{ return nullptr; }

	ScriptNode	*	parent() const { return _parent; }
	void			setParent(ScriptNode * newParent) { _parent = newParent; }

	std::string		toolTip() const { return _toolTip; }
	void			setToolTip(const std::string & tip) { _toolTip = tip; }

	static ScriptNode * fromJson(const Json::Value & json, ScriptNode * parent = nullptr);
	static void			deleteTree(ScriptNode * node);

	std::string		nodeTypeString() const;
	static Type		typeFromString(const std::string & str);

protected:
	ScriptNode	* _parent = nullptr;
	std::string	_toolTip;
};

class ScriptNodeOperator : public ScriptNode
{
	Q_OBJECT

public:
	ScriptNodeOperator(const std::string & op, bool vertical, ScriptNode * parent = nullptr);

	Type			type() const override { return _vertical ? Type::OperatorVertical : Type::Operator; }
	Json::Value		toJson() const override;
	std::string		toR(const ScriptColumnTypeProvider * typeProvider = nullptr) const override;
	stringvec		dragKeys() const override;
	bool			isComplete() const override;

	ScriptNode	*	leftChild()		const override { return _left; }
	ScriptNode	*	rightChild()	const override { return _right; }
	int				childCount()	const override { return 2; }
	ScriptNode	*	childAt(int i)	const override { return i == 0 ? _left : _right; }

	const std::string & op() const { return _op; }
	bool isVertical() const { return _vertical; }

	void setLeft(ScriptNode * node);
	void setRight(ScriptNode * node);

	stringvec dropKeysLeft() const;
	stringvec dropKeysRight() const;

private:
	std::string		_op;
	bool			_vertical;
	ScriptNode	*	_left	= nullptr,
				*	_right	= nullptr;
};

class ScriptNodeFunction : public ScriptNode
{
	Q_OBJECT

public:
	struct Argument
	{
		std::string		name;
		stringvec		dropKeys;
		bool			optional = false;
		ScriptNode	*	value = nullptr;
	};

	ScriptNodeFunction(const std::string & functionName, ScriptNode * parent = nullptr);
	ScriptNodeFunction(const std::string & functionName, const std::vector<Argument> & args, ScriptNode * parent = nullptr);

	Type			type() const override { return Type::Function; }
	Json::Value		toJson() const override;
	std::string		toR(const ScriptColumnTypeProvider * typeProvider = nullptr) const override;
	stringvec		dragKeys() const override;
	bool			isComplete() const override;

	int				childCount()	const override { return static_cast<int>(_arguments.size()); }
	ScriptNode	*	childAt(int i)	const override { return _arguments.at(i).value; }

	const std::string & functionName() const { return _functionName; }
	const std::vector<Argument> & arguments() const { return _arguments; }

	void addArgument(const Argument & arg);
	void setArgumentValue(int index, ScriptNode * node);
	int argumentIndex(const std::string & name) const;

private:
	std::string				_functionName;
	std::vector<Argument>	_arguments;
};

class ScriptNodeRowFunction : public ScriptNode
{
	Q_OBJECT

public:
	ScriptNodeRowFunction(const std::string & functionName, ScriptNode * parent = nullptr);

	Type			type() const override { return Type::RowFunction; }
	Json::Value		toJson() const override;
	std::string		toR(const ScriptColumnTypeProvider * typeProvider = nullptr) const override;
	stringvec		dragKeys() const override;
	bool			isComplete() const override;

	int				childCount()	const override { return static_cast<int>(_children.size()); }
	ScriptNode	*	childAt(int i)	const override { return _children.at(i); }

	const std::string & functionName() const { return _functionName; }
	const std::vector<ScriptNode*> & children() const { return _children; }

	void setChild(int index, ScriptNode * node);
	void addChild(ScriptNode * node);
	void removeChildAt(int index);
	int childCountFilled() const;

private:
	std::string					_functionName;
	std::vector<ScriptNode*>	_children;
};

class ScriptNodeColumn : public ScriptNode
{
	Q_OBJECT

public:
	ScriptNodeColumn(const std::string & columnName, int columnTypeUser = -1, int columnTypeDrop = -1, ScriptNode * parent = nullptr);

	Type			type() const override { return Type::Column; }
	Json::Value		toJson() const override;
	std::string		toR(const ScriptColumnTypeProvider * typeProvider = nullptr) const override;
	stringvec		dragKeys() const override;
	bool			isComplete() const override { return true; }

	const std::string & columnName() const { return _columnName; }
	int columnTypeUser() const { return _columnTypeUser; }
	int columnTypeDrop() const { return _columnTypeDrop; }
	const std::string & dataSetName() const { return _dataSetName; }

	void setColumnName(const std::string & name) { _columnName = name; }
	void setColumnTypeUser(int t);
	void setColumnTypeDrop(int t);
	void setDataSetName(const std::string & name) { _dataSetName = name; }

	int effectiveColumnType(int actualColumnType) const;

private:
	std::string	_columnName;
	int			_columnTypeUser = -1,
				_columnTypeDrop = -1;
	std::string	_dataSetName;
};

class ScriptNodeLiteral : public ScriptNode
{
	Q_OBJECT

public:
	ScriptNodeLiteral(Type literalType, ScriptNode * parent = nullptr);

	Type			type() const override { return _literalType; }
	Json::Value		toJson() const override;
	std::string		toR(const ScriptColumnTypeProvider * typeProvider = nullptr) const override;
	stringvec		dragKeys() const override;
	bool			isComplete() const override { return true; }

	double			numberValue() const { return _numberValue; }
	bool			boolValue() const { return _boolValue; }
	const std::string & stringValue() const { return _stringValue; }

	void setNumberValue(double v) { _numberValue = v; }
	void setBoolValue(bool v) { _boolValue = v; }
	void setStringValue(const std::string & v) { _stringValue = v; }

private:
	Type		_literalType;
	double		_numberValue = 0;
	bool		_boolValue = false;
	std::string	_stringValue;
};

#endif // SCRIPTNODE_H
