#ifndef SCRIPTCONSTRUCTORREGISTRY_H
#define SCRIPTCONSTRUCTORREGISTRY_H

#include <string>
#include <vector>
#include <map>
#include <QString>
#include "utils.h"
#include "enumutilities.h"

namespace ScriptConstructorEnums
{
	DECLARE_ENUM(ScriptConstructorMode, Filter, ComputedColumn, ComputedDataSet)

	Q_NAMESPACE
	Q_ENUM_NS(ScriptConstructorMode)
}

using ScriptConstructorEnums::ScriptConstructorMode;

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
	QString							toolTip;
	std::string						image;
	std::vector<ScriptParamDef>		params;
	bool							variadic		= false;
	bool							isRowFunction	= false;
	bool							operatorBarOnly	= false;
	bool							logicalSuffix	= false;

	// Data-driven per-function behaviour (replaces the old name-based switches); set
	// explicitly per function in the registry constructor table.
	stringvec						dragKeysData	= {"number"};	///< keys a placed node of this function offers when dragged
	bool							naRm			= false;		///< R call gets ", na.rm=TRUE" appended
	bool							radix			= false;		///< rendered as root head + overline (sqrt)
	bool							parensSingleArg	= true;			///< a single argument gets parentheses (all functions but abs)
	std::string						barAfter;						///< operator bar: insert directly after this operator; empty with operatorBarOnly means "append after the operator list"

	stringvec						dragKeys() const					{ return dragKeysData; }
	bool							addsNaRm() const						{ return naRm; }
	QString							toolTipForMode(ScriptConstructorMode mode) const;
};

struct ScriptOperatorDef
{
	std::string		op;
	QString			toolTip;
	std::string		image;
	bool			vertical		= false;
	bool			logicalSuffix	= false;

	// Per-side drop keys, one set per mode group; most operators accept the same keys in
	// both modes (only %|% differs). Set per operator in the registry constructor table.
	stringvec		dropKeysLeftFilter,	dropKeysRightFilter;	///< accepted keys per side in Filter mode
	stringvec		dropKeysLeftColumn,	dropKeysRightColumn;	///< accepted keys per side in ComputedColumn/ComputedDataSet

	bool			booleanResultFilter	= false;				///< returns logicals in Filter mode (can root a filter formula)
	bool			booleanResultColumn	= false;				///< returns logicals in the column modes
	bool			keysMirrored		= false;				///< ==/!= style: both sides accept the same keys

	stringvec		dropKeysLeft(	ScriptConstructorMode mode) const;
	stringvec		dropKeysRight(	ScriptConstructorMode mode) const;
	bool			mirrorKeys() const;
	bool			returnsBoolean(	ScriptConstructorMode mode) const;
	stringvec		dragKeys(		ScriptConstructorMode mode) const;
	QString			toolTipForMode(	ScriptConstructorMode mode) const;
};

class ScriptConstructorRegistry
{
public:
	static const ScriptConstructorRegistry & instance();

	const std::vector<ScriptOperatorDef>	& operators()		const { return _operators;		}
	const std::vector<ScriptFunctionDef>	& functions()		const { return _functions;		}
	const std::vector<ScriptFunctionDef>	& rowFunctions()	const { return _rowFunctions;	}

	const ScriptOperatorDef		* operatorDef(	const std::string & op, bool vertical = false)	const;
	const ScriptFunctionDef		* functionDef(	const std::string & name)	const;
	const ScriptFunctionDef		* rowFunctionDef(const std::string & name)	const;

	std::vector<ScriptFunctionDef>	functionsForMode(ScriptConstructorMode mode) const;
	std::vector<ScriptOperatorDef>	operatorsForMode(ScriptConstructorMode mode) const;

	static stringvec				dropKeysForColumnType(int columnType);

	/// Shared drop keys for row-function argument slots (row functions combine numeric columns).
	static const stringvec &		rowFunctionKeys();

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
