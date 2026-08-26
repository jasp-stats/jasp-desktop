#include "scriptconstructorregistry.h"
#include "columntype.h"

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

stringvec ScriptFunctionDef::dragKeys() const
{
	static const stringvec booleanKeys	= {"boolean"},
							numberKeys	= {"number"},
							ifElseKeys	= {"string", "number", "boolean"};

	//NB: matches QML Function.qml `isIfElse: functionName === "ifelse"` (lowercase only)
	if(name == "ifelse")								return ifElseKeys;
	if(name == "!" || name == "hasSubstring" || name == "is.na")
												return booleanKeys;
	return numberKeys;
}

bool ScriptFunctionDef::addsNaRm() const
{
	static const stringset naRmFunctions = {"mean", "sd", "var", "sum", "prod", "min", "max", "median"};

	return naRmFunctions.count(name) > 0;
}

stringvec ScriptOperatorDef::dropKeysLeft(ScriptConstructorMode mode) const
{
	static const stringvec numberKeys			= {"number"},
							booleanKeys			= {"boolean"},
							everythingKeys		= {"boolean", "string", "number"},
							numberCompareKeys	= {"number", "ordered"};

	if(op == "<" || op == ">" || op == "<=" || op == ">=")	return numberCompareKeys;
	if(op == "%|%")											return mode == ScriptConstructorMode::Filter ? booleanKeys : numberKeys;
	if(op == "==" || op == "!=")							return everythingKeys;
	if(op == "&" || op == "|")								return booleanKeys;
	return numberKeys;
}

stringvec ScriptOperatorDef::dropKeysRight(ScriptConstructorMode mode) const
{
	static const stringvec numberKeys			= {"number"},
							booleanKeys			= {"boolean"},
							everythingKeys		= {"boolean", "string", "number"},
							numberCompareKeys	= {"number", "ordered"},
							conditionalRight	= {"string", "boolean"};

	if(op == "<" || op == ">" || op == "<=" || op == ">=")	return numberCompareKeys;
	if(op == "%|%")											return conditionalRight;
	if(op == "==" || op == "!=")							return everythingKeys;
	if(op == "&" || op == "|")								return booleanKeys;
	return numberKeys;
}

bool ScriptOperatorDef::mirrorKeys() const
{
	return op == "==" || op == "!=";
}

bool ScriptOperatorDef::returnsBoolean(ScriptConstructorMode mode) const
{
	static const stringset booleanOps		= {"&", "|"},
							numberCompare	= {"<", ">", "<=", ">="},
							everythingOps	= {"==", "!="};

	if(booleanOps.count(op) || numberCompare.count(op) || everythingOps.count(op))
		return true;

	if(op == "%|%")
		return mode == ScriptConstructorMode::Filter;

	return false;
}

stringvec ScriptOperatorDef::dragKeys(ScriptConstructorMode mode) const
{
	return returnsBoolean(mode) ? stringvec{"boolean"} : stringvec{"number"};
}

ScriptConstructorRegistry::ScriptConstructorRegistry()
{
	auto addOp = [this](const std::string & op, const std::string & toolTip, const std::string & image = "", bool vertical = false)
	{
		_operatorIndex[op + (vertical ? "V" : "")] = _operators.size();
		_operators.push_back({op, toolTip, image, vertical});
	};

	addOp("+",	"Addition",								"plus.png");
	addOp("-",	"Subtraction",							"minus.png");
	addOp("*",	"Multiplication",						"multiply.png");
	addOp("/",	"Division",								"divide.png", true);
	addOp("/",	"Division",								"");
	addOp("^",	"Power (2^3 returns 8)",					"");
	addOp("%%",	"Modulo: returns the remainder of a division. 3%2 returns 1",	"modulo.png");
	addOp("==",	"Equality: returns logicals",			"equal.png");
	addOp("!=",	"Inequality: returns logicals",			"notEqual.png");
	addOp("<",	"Less than: returns logicals",			"lessThan.png");
	addOp("<=",	"Less than or equal to: returns logicals",	"lessThanEqual.png");
	addOp(">",	"Greater than: returns logicals",		"greaterThan.png");
	addOp(">=",	"Greater than or equal to: returns logicals",	"greaterThanEqual.png");
	addOp("&",	"And: returns logicals",					"and.png");
	addOp("|",	"Or: returns logicals",					"or.png");
	addOp("%|%", "Split: applies filter separately to each subgroup",	"ConditionBy.png");

	auto addFunc = [this](const std::string & name, const std::string & friendlyName, const std::string & toolTip, const std::vector<ScriptParamDef> & params, const std::string & image = "")
	{
		_functionIndex[name] = _functions.size();
		_functions.push_back({name, friendlyName, toolTip, image, params, false, false});
	};

	auto P = [](const std::string & name, const stringvec & keys) { return ScriptParamDef::fromRaw(name, keys); };

	static const stringvec numKeys		= {"number"},
							boolKeys	= {"boolean"},
							strKeys		= {"string"},
							boolStrNum	= {"boolean", "string", "number"},
							strNum		= {"string", "number"},
							strBoolNum	= {"string", "boolean", "number"};

	addFunc("abs",		"", "absolute value",					{P("values", numKeys)});
	addFunc("sd",		"", "standard deviation",				{P("values", numKeys)});
	addFunc("var",		"", "variance",						{P("values", numKeys)});
	addFunc("sum",		"", "summation",						{P("values", numKeys)});
	addFunc("prod",		"", "product of values",				{P("values", numKeys)});
	addFunc("zScores",	"", "Standardizes the variable",		{P("values", numKeys)});
	addFunc("min",		"", "returns minimum of values",		{P("values", numKeys)});
	addFunc("max",		"", "returns maximum of values",		{P("values", numKeys)});
	addFunc("mean",		"", "mean",							{P("values", numKeys)});
	addFunc("sign",		"", "returns the sign of values",		{P("values", numKeys)});
	addFunc("round",	"", "rounds y to n decimals",			{P("y", numKeys), P("n", numKeys)});
	addFunc("length",	"", "returns number of elements in y",	{P("y", strNum)});
	addFunc("median",	"", "median",							{P("values", numKeys)});
	addFunc("ifelse",	"", "if-else statement",				{P("test", boolKeys), P("then", boolStrNum), P("else", boolStrNum)});
	addFunc("hasSubstring", "", "returns true if string contains substring at least once", {P("string", strKeys), P("substring", strKeys)});
	addFunc("is.na",	"", "Combine with not-operator to filter out rows with missing values (NA) for a column.", {P("y", strBoolNum)});

	addFunc("log",		"", "natural logarithm",				{P("y", numKeys)});
	addFunc("log2",		"log\u2082", "base 2 logarithm",		{P("y", numKeys)});
	addFunc("log10",	"log\u2081\u2080", "base 10 logarithm",	{P("y", numKeys)});
	addFunc("logb",		"", "logarithm of y in 'base'",		{P("y", numKeys), P("base", numKeys)});
	addFunc("exp",		"", "exponential",						{P("y", numKeys)});
	addFunc("fishZ",	"", "Fisher's Z-transform (i.e., the inverse hyperbolic tangent) to transform correlations, numbers between -1 and 1 to the real line", {P("y", numKeys)});
	addFunc("invFishZ",	"fishZ\u207B\u00B9", "Inverse Fisher's Z-transform (i.e., the hyperbolic tangent) to transform real numbers to numbers between -1 and 1", {P("y", numKeys)});
	addFunc("logit",	"", "Logit transform (i.e., the inverse of the standard logit function, or log-odds transform) converts numbers between 0 and 1 to the real line.", {P("y", numKeys)});
	addFunc("invLogit",	"logit\u207B\u00B9", "Inverse logit transform (i.e., the standard logit function) converts numbers on the real line to numbers between 0 and 1.", {P("y", numKeys)});
	addFunc("BoxCox",	"", "Two-parameter Box-Cox transform (transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like.", {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)});
	addFunc("BoxCoxAuto", "", "Two-parameter Box-Cox transform with an automatic determination of the shape parameter lambda, according to one of the three of methods:'loglik', 'sd', or 'movingRange'. The search for optimal lambda is bounded within 'lower' and 'upper' limits.", {P("y", numKeys), P("?predictor", numKeys), P("?groupSize", numKeys), P("method", strKeys), P("lower", numKeys), P("upper", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)});
	addFunc("invBoxCox", "BoxCox\u207B\u00B9", "Inverse two-parameter Box-Cox transform.", {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)});
	addFunc("powerTransform", "", "Two-parameter power transform (scale-invariant Box-Box; transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like.", {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys)});
	addFunc("powerTransformAuto", "", "Two-parameter power transform with an automatic determination of the shape parameter lambda. The search for optimal lambda is bounded within 'lower' and 'upper' limits.", {P("y", numKeys), P("?predictor", numKeys), P("?groupSize", numKeys), P("lower", numKeys), P("upper", numKeys), P("shift", numKeys)});
	addFunc("YeoJohnson", "", "Yeo-Johnson transform (transforms any real values) to stabilize variance and attempt to make the data more normal distribution-like.", {P("y", numKeys), P("lambda", numKeys)});
	addFunc("YeoJohnsonAuto", "", "Yeo-Johnson transform (transforms any real values) with an automatic determination of the shape parameter lambda. The search for optimal lambda is bounded within 'lower' and 'upper' limits.", {P("y", numKeys), P("lower", numKeys), P("upper", numKeys)});
	addFunc("Johnson",	"", "Johnson transform (transforms any real values). The search for optimal parameter is bounded within 'lower' and 'upper' limits.", {P("y", numKeys), P("lower", numKeys), P("upper", numKeys)});

	addFunc("cut",		"", "break your data up in numBreaks levels", {P("values", numKeys), P("numBreaks", numKeys)});
	addFunc("replaceNA", "", "replace any missing values (NA) in column by the value in replaceWith", {P("column", strBoolNum), P("replaceWith", strBoolNum)});
	addFunc("ifElse",	"", "if-else statement",				{P("test", boolKeys), P("then", boolStrNum), P("else", boolStrNum)});

	addFunc("normalDist",	"", "generates data from a Gaussian distribution with specified mean and standard deviation sd", {P("mean", numKeys), P("sd", numKeys)});
	addFunc("tDist",		"", "generates data from t distribution with degrees of freedom df and non-centrality parameter ncp", {P("df", numKeys), P("ncp", numKeys)});
	addFunc("chiSqDist",	"", "generates data from a chi-squared distribution with degrees of freedom df and non-centrality parameter ncp", {P("df", numKeys), P("ncp", numKeys)});
	addFunc("fDist",		"", "generates data from an F distribution with specified degrees of freedoms df1, df2 and non-centrality parameter ncp", {P("df1", numKeys), P("df2", numKeys), P("ncp", numKeys)});
	addFunc("binomDist",	"", "generates data from a binomial distribution with specified trials and probability prob", {P("trials", numKeys), P("prob", numKeys)});
	addFunc("negBinomDist",	"", "generates data from a negative binomial distribution with specified trials and probability prob", {P("targetTrial", numKeys), P("prob", numKeys)});
	addFunc("geomDist",		"", "generates data from a geometric distribution with specified probability prob", {P("prob", numKeys)});
	addFunc("poisDist",		"", "generates data from a Poisson distribution with specified rate lambda", {P("lambda", numKeys)});
	addFunc("betaDist",		"", "generates data from a beta distribution with specified shapes alpha and beta", {P("alpha", numKeys), P("beta", numKeys)});
	addFunc("unifDist",		"", "generates data from a uniform distribution between min and max", {P("min", numKeys), P("max", numKeys)});
	addFunc("gammaDist",	"", "generates data from a gamma distribution with specified shape and scale", {P("shape", numKeys), P("scale", numKeys)});
	addFunc("expDist",		"", "generates data from an exponential distribution with specified rate", {P("rate", numKeys)});
	addFunc("logNormDist",	"", "generates data from a log-normal distribution with specified logarithmic mean meanLog and standard deviation sdLog", {P("meanLog", numKeys), P("sdLog", numKeys)});
	addFunc("weibullDist",	"", "generates data from a Weibull distribution with specified shape and scale", {P("shape", numKeys), P("scale", numKeys)});

	auto addRowFunc = [this](const std::string & name, const std::string & toolTip)
	{
		_rowFunctionIndex[name] = _rowFunctions.size();
		_rowFunctions.push_back({name, name, toolTip, "", {}, true, true});
	};

	addRowFunc("rowMean",		"Rowwise mean");
	addRowFunc("rowSum",		"Rowwise sum");
	addRowFunc("rowSD",			"Rowwise standard deviation");
	addRowFunc("rowVariance",	"Rowwise variance");
	addRowFunc("rowMedian",		"Rowwise median");
	addRowFunc("rowMin",		"Rowwise minimum");
	addRowFunc("rowMax",		"Rowwise maximum");
}

const ScriptConstructorRegistry & ScriptConstructorRegistry::instance()
{
	static ScriptConstructorRegistry registry;
	return registry;
}

const ScriptOperatorDef * ScriptConstructorRegistry::operatorDef(const std::string & op) const
{
	for(const ScriptOperatorDef & def : _operators)
		if(def.op == op)
			return &def;

	return nullptr;
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
	static const stringset filterOnlyFunctions = {"ifelse"};

	std::vector<ScriptFunctionDef> out;

	for(const ScriptFunctionDef & def : _functions)
	{
		if(mode == ScriptConstructorMode::Filter && def.name == "ifElse")	continue;
		if(mode != ScriptConstructorMode::Filter && filterOnlyFunctions.count(def.name))	continue;

		out.push_back(def);
	}

	return out;
}

std::vector<ScriptOperatorDef> ScriptConstructorRegistry::operatorsForMode(ScriptConstructorMode) const
{
	return _operators;
}

stringvec ScriptConstructorRegistry::dropKeysForColumnType(int colType)
{
	switch(colType)
	{
	case 1:		return {"number"};
	case 2:		return {"string", "ordered"};
	default:	return {"string"};
	}
}

std::string ScriptConstructorRegistry::columnTypeString(int colType)
{
	switch(colType)
	{
	case 1:		return "scale";
	case 2:		return "ordinal";
	default:	return "nominal";
	}
}
