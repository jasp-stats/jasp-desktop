#ifndef SCRIPTCONSTRUCTORREGISTRY_H
#define SCRIPTCONSTRUCTORREGISTRY_H

#include <string>
#include <vector>
#include <map>
#include "utils.h"

enum class ScriptConstructorMode { Filter, ComputedColumn, ComputedDataSet };

struct ScriptParamDef
{
	std::string		name;
	stringvec		dropKeys;
	bool			optional	= false;

	static ScriptParamDef fromRaw(const std::string & rawName, const stringvec & rawDropKeys);
};

struct ScriptFunctionDef
{
	std::string						name;
	std::string						friendlyName;
	std::string						toolTip;
	std::string						image;
	std::vector<ScriptParamDef>		params;
	bool							variadic		= false;
	bool							isRowFunction	= false;
	bool							operatorBarOnly	= false;

	stringvec						dragKeys() const;
	bool							addsNaRm() const;
};

struct ScriptOperatorDef
{
	std::string		op;
	std::string		toolTip;
	std::string		image;
	bool			vertical = false;

	stringvec		dropKeysLeft(	ScriptConstructorMode mode) const;
	stringvec		dropKeysRight(	ScriptConstructorMode mode) const;
	bool			mirrorKeys() const;
	bool			returnsBoolean(	ScriptConstructorMode mode) const;
	stringvec		dragKeys(		ScriptConstructorMode mode) const;
};

class ScriptConstructorRegistry
{
public:
	static const ScriptConstructorRegistry & instance();

	const std::vector<ScriptOperatorDef>	& operators()		const { return _operators;		}
	const std::vector<ScriptFunctionDef>	& functions()		const { return _functions;		}
	const std::vector<ScriptFunctionDef>	& rowFunctions()	const { return _rowFunctions;	}

	const ScriptOperatorDef		* operatorDef(	const std::string & op)		const;
	const ScriptFunctionDef		* functionDef(	const std::string & name)	const;
	const ScriptFunctionDef		* rowFunctionDef(const std::string & name)	const;

	std::vector<ScriptFunctionDef>	functionsForMode(ScriptConstructorMode mode) const;
	std::vector<ScriptOperatorDef>	operatorsForMode(ScriptConstructorMode mode) const;

	static stringvec				dropKeysForColumnType(int columnType);
	static std::string				columnTypeString(int columnType);

private:
	ScriptConstructorRegistry();

	std::vector<ScriptOperatorDef>		_operators;
	std::vector<ScriptFunctionDef>		_functions;
	std::vector<ScriptFunctionDef>		_rowFunctions;
	std::map<std::string, size_t>		_operatorIndex,
										_functionIndex,
										_rowFunctionIndex;
};

#endif // SCRIPTCONSTRUCTORREGISTRY_H
