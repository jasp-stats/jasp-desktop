#include "scriptconstructorregistry.h"
#include "columntype.h"
#include <QCoreApplication>

ScriptParamDef ScriptParamDef::fromRaw(const std::string & rawName, const stringvec & rawDropKeys)
{
	ScriptParamDef out;
	out.name		= rawName;
	out.dropKeys	= rawDropKeys;
	out.optional	= rawName.size() > 0 && rawName[0] == '?';

	if(out.optional)
		out.name = rawName.substr(1);

	return out;
}

static QString logicalSuffixToolTip(const QString & toolTip, bool logicalSuffix, ScriptConstructorMode mode)
{
	const QString translated = QCoreApplication::translate("ScriptConstructorRegistry", toolTip.toUtf8().constData());

	if(!logicalSuffix)
		return translated;

	return translated.arg(mode == ScriptConstructorMode::Filter
		? QCoreApplication::translate("ScriptConstructorRegistry", "returns logicals and can be the root of a filter formula")
		: QCoreApplication::translate("ScriptConstructorRegistry", "returns logicals"));
}

stringvec ScriptOperatorDef::dropKeysLeft(ScriptConstructorMode mode) const
{
	return mode == ScriptConstructorMode::Filter ? dropKeysLeftFilter : dropKeysLeftColumn;
}

stringvec ScriptOperatorDef::dropKeysRight(ScriptConstructorMode mode) const
{
	return mode == ScriptConstructorMode::Filter ? dropKeysRightFilter : dropKeysRightColumn;
}

bool ScriptOperatorDef::mirrorKeys() const
{
	return keysMirrored;
}

bool ScriptOperatorDef::returnsBoolean(ScriptConstructorMode mode) const
{
	return mode == ScriptConstructorMode::Filter ? booleanResultFilter : booleanResultColumn;
}

stringvec ScriptOperatorDef::dragKeys(ScriptConstructorMode mode) const
{
	return returnsBoolean(mode) ? stringvec{"boolean"} : stringvec{"number"};
}

QString ScriptOperatorDef::toolTipForMode(ScriptConstructorMode mode) const
{
	return logicalSuffixToolTip(toolTip, logicalSuffix, mode);
}

bool ScriptFunctionDef::inPalette(ScriptConstructorMode mode) const
{
	// operatorBarOnly functions (sqrt, !) are offered in the operator bar instead.
	return !operatorBarOnly && (mode == ScriptConstructorMode::Filter ? inFilterPalette : inColumnPalette);
}

QString ScriptFunctionDef::toolTipForMode(ScriptConstructorMode mode) const
{
	return logicalSuffixToolTip(toolTip, logicalSuffix, mode);
}

ScriptConstructorRegistry::ScriptConstructorRegistry()
{
	// Drop-key vocabulary shared by the operator and function tables below.
	static const stringvec numKeys			= {"number"},
							boolKeys		= {"boolean"},
							strKeys			= {"string"},
							boolStrNum		= {"boolean", "string", "number"},
							strBoolNum		= {"string", "boolean", "number"},
							ifElseKeys		= {"string", "number", "boolean"},
							numberCompareKeys	= {"number", "ordered"},
							conditionalRight	= {"string", "boolean"};

	// Operators and functions are registered via designated initializers so every field
	// (drop keys, boolean result, na.rm, radix, ...) is named at the call site.

	// Fill the unset per-mode key vectors: an operator that accepts the same keys in both
	// modes (all but %|%) only sets the Filter vectors; a fully unset side defaults to numeric.
	auto fillKeys = [](stringvec & filterKeys, stringvec & columnKeys)
	{
		if(filterKeys.empty() && columnKeys.empty())	{ filterKeys = columnKeys = numKeys;	}
		else if(filterKeys.empty())						{ filterKeys = columnKeys;				}
		else if(columnKeys.empty())						{ columnKeys = filterKeys;				}
	};

	auto addOp = [this, fillKeys](ScriptOperatorDef def)
	{
		fillKeys(def.dropKeysLeftFilter,	def.dropKeysLeftColumn);
		fillKeys(def.dropKeysRightFilter,	def.dropKeysRightColumn);
		_operatorIndex[def.op + (def.vertical ? "V" : "")] = _operators.size();
		_operators.push_back(std::move(def));
	};

	addOp({ .op = "+",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Addition"),								.image = "plus.png"	});
	addOp({ .op = "-",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Subtraction"),							.image = "minus.png"	});
	addOp({ .op = "*",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Multiplication"),						.image = "multiply.png"	});
	addOp({ .op = "/",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Division"),								.image = "divide.png", .vertical = true	});
	addOp({ .op = "/",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Division")								});
	addOp({ .op = "^",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Power (2^3 returns 8)")					});
	addOp({ .op = "%%",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Modulo: returns the remainder of a division. 3%2 returns 1"),	.image = "modulo.png"	});
	addOp({ .op = "==",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Equality: %1"),							.image = "equal.png",			.logicalSuffix = true,	.dropKeysLeftFilter = boolStrNum,	.dropKeysRightFilter = boolStrNum,	.booleanResultFilter = true,	.booleanResultColumn = true,	.keysMirrored = true	});
	addOp({ .op = "!=",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Inequality: %1"),						.image = "notEqual.png",		.logicalSuffix = true,	.dropKeysLeftFilter = boolStrNum,	.dropKeysRightFilter = boolStrNum,	.booleanResultFilter = true,	.booleanResultColumn = true,	.keysMirrored = true	});
	addOp({ .op = "<",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Less than: %1"),							.image = "lessThan.png",		.logicalSuffix = true,	.dropKeysLeftFilter = numberCompareKeys,	.dropKeysRightFilter = numberCompareKeys,	.booleanResultFilter = true,	.booleanResultColumn = true	});
	addOp({ .op = "<=",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Less than or equal to: %1"),			.image = "lessThanEqual.png",	.logicalSuffix = true,	.dropKeysLeftFilter = numberCompareKeys,	.dropKeysRightFilter = numberCompareKeys,	.booleanResultFilter = true,	.booleanResultColumn = true	});
	addOp({ .op = ">",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Greater than: %1"),						.image = "greaterThan.png",		.logicalSuffix = true,	.dropKeysLeftFilter = numberCompareKeys,	.dropKeysRightFilter = numberCompareKeys,	.booleanResultFilter = true,	.booleanResultColumn = true	});
	addOp({ .op = ">=",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Greater than or equal to: %1"),			.image = "greaterThanEqual.png",	.logicalSuffix = true,	.dropKeysLeftFilter = numberCompareKeys,	.dropKeysRightFilter = numberCompareKeys,	.booleanResultFilter = true,	.booleanResultColumn = true	});
	addOp({ .op = "&",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "And: %1"),								.image = "and.png",				.logicalSuffix = true,	.dropKeysLeftFilter = boolKeys,		.dropKeysRightFilter = boolKeys,		.booleanResultFilter = true,	.booleanResultColumn = true	});
	addOp({ .op = "|",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Or: %1"),								.image = "or.png",				.logicalSuffix = true,	.dropKeysLeftFilter = boolKeys,		.dropKeysRightFilter = boolKeys,		.booleanResultFilter = true,	.booleanResultColumn = true	});
	// %|% is the only mode-dependent operator: its left side takes booleans in filter mode
	// and numbers in column mode, and it only returns logicals in filter mode.
	addOp({ .op = "%|%",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Split: applies filter separately to each subgroup"),	.image = "ConditionBy.png",	.dropKeysLeftFilter = boolKeys,	.dropKeysRightFilter = conditionalRight,	.dropKeysLeftColumn = numKeys,	.dropKeysRightColumn = conditionalRight,	.booleanResultFilter = true	});

	auto addFunc = [this](ScriptFunctionDef def)
	{
		_functionIndex[def.name] = _functions.size();
		_functions.push_back(std::move(def));
	};

	auto P = [](const std::string & name, const stringvec & keys) { return ScriptParamDef::fromRaw(name, keys); };

	// Function palettes (as the old FilterWindow.qml / ComputeColumnWindow.qml lists): the filter
	// offers abs .. is.na; the computed-column constructors also offer the transforms, cut/replaceNA
	// and the random-data generators below (inFilterPalette = false), and ifElse instead of ifelse.

	addFunc({ .name = "abs",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "absolute value"),					.params = {P("values", numKeys)},	.parensSingleArg = false	});
	addFunc({ .name = "sd",				.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "standard deviation"),				.image = "sigma.png",	.params = {P("values", numKeys)},		.naRm = true	});
	addFunc({ .name = "var",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "variance"),						.image = "variance.png",	.params = {P("values", numKeys)},	.naRm = true	});
	addFunc({ .name = "sum",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "summation"),						.image = "sum.png",	.params = {P("values", numKeys)},			.naRm = true	});
	addFunc({ .name = "prod",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "product of values"),				.image = "product.png",	.params = {P("values", numKeys)},		.naRm = true	});
	addFunc({ .name = "zScores",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Standardizes the variable"),		.params = {P("values", numKeys)}	});
	addFunc({ .name = "min",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns minimum of values"),		.params = {P("values", numKeys)},	.naRm = true	});
	addFunc({ .name = "max",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns maximum of values"),		.params = {P("values", numKeys)},	.naRm = true	});
	addFunc({ .name = "mean",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "mean"),							.params = {P("values", numKeys)},	.naRm = true	});
	addFunc({ .name = "sign",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns the sign of values"),		.params = {P("values", numKeys)}	});
	addFunc({ .name = "round",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "rounds y to n decimals"),			.params = {P("y", numKeys), P("n", numKeys)}	});
	addFunc({ .name = "length",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns number of elements in y"),	.params = {P("y", strBoolNum)}	});
	addFunc({ .name = "median",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "median"),							.params = {P("values", numKeys)},	.naRm = true	});
	//NB: dragKeys matches QML Function.qml `isIfElse: functionName === "ifelse"` (lowercase only)
	addFunc({ .name = "ifelse",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "if-else statement"),				.params = {P("test", boolKeys), P("then", boolStrNum), P("else", boolStrNum)},	.dragKeysData = ifElseKeys,	.inColumnPalette = false	});
	addFunc({ .name = "hasSubstring",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns true if string contains substring at least once"),	.params = {P("string", strKeys), P("substring", strKeys)},	.dragKeysData = boolKeys	});
	addFunc({ .name = "is.na",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Combine with not-operator to filter out rows with missing values (NA) for a column."),			.params = {P("y", strBoolNum)},	.dragKeysData = boolKeys	});

	// sqrt and ! live only in the operator bar (interspersed with the operators, positioned
	// through barAfter), not in the right-hand function palette; ! has no barAfter and is
	// therefore appended after the operator list.
	addFunc({ .name = "sqrt",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Square root"),						.image = "rootHead.png",	.params = {P("value(s)", numKeys)},	.operatorBarOnly = true,	.radix = true,	.barAfter = "^"	});
	addFunc({ .name = "!",				.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Not: %1"),							.image = "negative.png",	.params = {P("logical(s)", boolKeys)},	.operatorBarOnly = true,	.logicalSuffix = true,	.dragKeysData = boolKeys	});

	addFunc({ .name = "log",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "natural logarithm"),				.params = {P("y", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "log2",			.friendlyName = "log\u2082",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "base 2 logarithm"),		.params = {P("y", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "log10",			.friendlyName = "log\u2081\u2080",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "base 10 logarithm"),	.params = {P("y", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "logb",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "logarithm of y in 'base'"),		.params = {P("y", numKeys), P("base", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "exp",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "exponential"),						.params = {P("y", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "fishZ",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Fisher's Z-transform (i.e., the inverse hyperbolic tangent) to transform correlations, numbers between -1 and 1 to the real line"),	.params = {P("y", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "invFishZ",		.friendlyName = "fishZ\u207B\u00B9",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Inverse Fisher's Z-transform (i.e., the hyperbolic tangent) to transform real numbers to numbers between -1 and 1"),	.params = {P("y", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "logit",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Logit transform (i.e., the inverse of the standard logit function, or log-odds transform) converts numbers between 0 and 1 to the real line."),	.params = {P("y", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "invLogit",		.friendlyName = "logit\u207B\u00B9",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Inverse logit transform (i.e., the standard logit function) converts numbers on the real line to numbers between 0 and 1."),	.params = {P("y", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "BoxCox",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Two-parameter Box-Cox transform (transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like."),	.params = {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "BoxCoxAuto",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Two-parameter Box-Cox transform with an automatic determination of the shape parameter lambda, according to one of the three of methods:'loglik', 'sd', or 'movingRange'. The search for optimal lambda is bounded within 'lower' and 'upper' limits."),	.params = {P("y", numKeys), P("?predictor", numKeys), P("?groupSize", numKeys), P("method", strKeys), P("lower", numKeys), P("upper", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "invBoxCox",		.friendlyName = "BoxCox\u207B\u00B9",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Inverse two-parameter Box-Cox transform."),	.params = {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "powerTransform",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Two-parameter power transform (scale-invariant Box-Box; transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like."),	.params = {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "powerTransformAuto",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Two-parameter power transform with an automatic determination of the shape parameter lambda. The search for optimal lambda is bounded within 'lower' and 'upper' limits."),	.params = {P("y", numKeys), P("?predictor", numKeys), P("?groupSize", numKeys), P("lower", numKeys), P("upper", numKeys), P("shift", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "YeoJohnson",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Yeo-Johnson transform (transforms any real values) to stabilize variance and attempt to make the data more normal distribution-like."),	.params = {P("y", numKeys), P("lambda", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "YeoJohnsonAuto",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Yeo-Johnson transform (transforms any real values) with an automatic determination of the shape parameter lambda. The search for optimal lambda is bounded within 'lower' and 'upper' limits."),	.params = {P("y", numKeys), P("lower", numKeys), P("upper", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "Johnson",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Johnson transform (transforms any real values). The search for optimal parameter is bounded within 'lower' and 'upper' limits."),	.params = {P("y", numKeys), P("lower", numKeys), P("upper", numKeys)},	.inFilterPalette = false	});

	addFunc({ .name = "cut",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "break your data up in numBreaks levels"),	.params = {P("values", numKeys), P("numBreaks", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "replaceNA",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "replace any missing values (NA) in column by the value in replaceWith"),	.params = {P("column", strBoolNum), P("replaceWith", strBoolNum)},	.inFilterPalette = false	});
	addFunc({ .name = "ifElse",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "if-else statement"),				.params = {P("test", boolKeys), P("then", boolStrNum), P("else", boolStrNum)},	.inFilterPalette = false	});

	addFunc({ .name = "normalDist",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a Gaussian distribution with specified mean and standard deviation sd"),	.params = {P("mean", numKeys), P("sd", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "tDist",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from t distribution with degrees of freedom df and non-centrality parameter ncp"),	.params = {P("df", numKeys), P("ncp", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "chiSqDist",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a chi-squared distribution with degrees of freedom df and non-centrality parameter ncp"),	.params = {P("df", numKeys), P("ncp", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "fDist",			.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from an F distribution with specified degrees of freedoms df1, df2 and non-centrality parameter ncp"),	.params = {P("df1", numKeys), P("df2", numKeys), P("ncp", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "binomDist",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a binomial distribution with specified trials and probability prob"),	.params = {P("trials", numKeys), P("prob", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "negBinomDist",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a negative binomial distribution with specified trials and probability prob"),	.params = {P("targetTrial", numKeys), P("prob", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "geomDist",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a geometric distribution with specified probability prob"),	.params = {P("prob", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "poisDist",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a Poisson distribution with specified rate lambda"),	.params = {P("lambda", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "betaDist",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a beta distribution with specified shapes alpha and beta"),	.params = {P("alpha", numKeys), P("beta", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "unifDist",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a uniform distribution between min and max"),	.params = {P("min", numKeys), P("max", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "gammaDist",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a gamma distribution with specified shape and scale"),	.params = {P("shape", numKeys), P("scale", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "expDist",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from an exponential distribution with specified rate"),	.params = {P("rate", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "logNormDist",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a log-normal distribution with specified logarithmic mean meanLog and standard deviation sdLog"),	.params = {P("meanLog", numKeys), P("sdLog", numKeys)},	.inFilterPalette = false	});
	addFunc({ .name = "weibullDist",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a Weibull distribution with specified shape and scale"),	.params = {P("shape", numKeys), P("scale", numKeys)},	.inFilterPalette = false	});

	// Row functions combine numeric columns; friendlyName/variadic/isRowFunction/dragKeys
	// are the same for all of them and are set by the lambda.
	auto addRowFunc = [this](ScriptFunctionDef def)
	{
		def.friendlyName	= def.name;
		def.variadic		= true;
		def.isRowFunction	= true;
		def.dragKeysData	= rowFunctionKeys();
		_rowFunctionIndex[def.name] = _rowFunctions.size();
		_rowFunctions.push_back(std::move(def));
	};

	addRowFunc({ .name = "rowMean",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise mean")	});
	addRowFunc({ .name = "rowSum",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise sum"),					.image = "sum.png"	});
	addRowFunc({ .name = "rowSD",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise standard deviation"),	.image = "sigma.png"	});
	addRowFunc({ .name = "rowVariance",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise variance"),				.image = "variance.png"	});
	addRowFunc({ .name = "rowMedian",	.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise median")	});
	addRowFunc({ .name = "rowMin",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise minimum")	});
	addRowFunc({ .name = "rowMax",		.toolTip = QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise maximum")	});
}

const ScriptConstructorRegistry & ScriptConstructorRegistry::instance()
{
	static ScriptConstructorRegistry registry;
	return registry;
}

const ScriptOperatorDef * ScriptConstructorRegistry::operatorDef(const std::string & op, bool vertical) const
{
	auto it = _operatorIndex.find(op + (vertical ? "V" : ""));
	return it != _operatorIndex.end() ? &_operators[it->second] : nullptr;
}

const ScriptFunctionDef * ScriptConstructorRegistry::functionDef(const std::string & name) const
{
	auto it = _functionIndex.find(name);
	return it != _functionIndex.end() ? &_functions[it->second] : nullptr;
}

const ScriptFunctionDef * ScriptConstructorRegistry::rowFunctionDef(const std::string & name) const
{
	auto it = _rowFunctionIndex.find(name);
	return it != _rowFunctionIndex.end() ? &_rowFunctions[it->second] : nullptr;
}

std::vector<ScriptFunctionDef> ScriptConstructorRegistry::functionsForMode(ScriptConstructorMode mode) const
{
	std::vector<ScriptFunctionDef> out;

	for(const ScriptFunctionDef & def : _functions)
		if(def.inPalette(mode))
			out.push_back(def);

	return out;
}

std::vector<ScriptOperatorDef> ScriptConstructorRegistry::operatorsForMode(ScriptConstructorMode) const
{
	return _operators;
}

stringvec ScriptConstructorRegistry::dropKeysForColumnType(int colType)
{
	switch(static_cast<columnType>(colType))
	{
	case columnType::scale:		return {"number"};
	case columnType::ordinal:	return {"string", "ordered"};
	default:					return {"string"}; // nominal (and anything else)
	}
}

const stringvec & ScriptConstructorRegistry::rowFunctionKeys()
{
	static const stringvec keys = {"number"};
	return keys;
}
