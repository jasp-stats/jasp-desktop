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

QString ScriptOperatorDef::toolTipForMode(ScriptConstructorMode mode) const
{
	if(!logicalSuffix)
		return toolTip;

	return toolTip.arg(mode == ScriptConstructorMode::Filter
		? ScriptConstructorRegistry::tr("returns logicals and can be the root of a filter formula")
		: ScriptConstructorRegistry::tr("returns logicals"));
}

QString ScriptFunctionDef::toolTipForMode(ScriptConstructorMode mode) const
{
	if(!logicalSuffix)
		return toolTip;

	return toolTip.arg(mode == ScriptConstructorMode::Filter
		? ScriptConstructorRegistry::tr("returns logicals and can be the root of a filter formula")
		: ScriptConstructorRegistry::tr("returns logicals"));
}

ScriptConstructorRegistry::ScriptConstructorRegistry()
{
	auto addOp = [this](const std::string & op, const QString & toolTip, const std::string & image = "", bool vertical = false, bool logicalSuffix = false)
	{
		_operatorIndex[op + (vertical ? "V" : "")] = _operators.size();
		_operators.push_back({op, toolTip, image, vertical, logicalSuffix});
	};

	addOp("+",		tr("Addition"),								"plus.png");
	addOp("-",		tr("Subtraction"),							"minus.png");
	addOp("*",		tr("Multiplication"),						"multiply.png");
	addOp("/",		tr("Division"),								"divide.png", true);
	addOp("/",		tr("Division"),								"");
	addOp("^",		tr("Power (2^3 returns 8)"),					"");
	addOp("%%",		tr("Modulo: returns the remainder of a division. 3%2 returns 1"),	"modulo.png");
	addOp("==",		tr("Equality: %1"),							"equal.png", false, true);
	addOp("!=",		tr("Inequality: %1"),						"notEqual.png", false, true);
	addOp("<",		tr("Less than: %1"),						"lessThan.png", false, true);
	addOp("<=",		tr("Less than or equal to: %1"),			"lessThanEqual.png", false, true);
	addOp(">",		tr("Greater than: %1"),						"greaterThan.png", false, true);
	addOp(">=",		tr("Greater than or equal to: %1"),			"greaterThanEqual.png", false, true);
	addOp("&",		tr("And: %1"),								"and.png", false, true);
	addOp("|",		tr("Or: %1"),								"or.png", false, true);
	addOp("%|%",	tr("Split: applies filter separately to each subgroup"),	"ConditionBy.png");

	auto addFunc = [this](const std::string & name, const std::string & friendlyName, const QString & toolTip, const std::vector<ScriptParamDef> & params, const std::string & image = "", bool operatorBarOnly = false, bool logicalSuffix = false)
	{
		_functionIndex[name] = _functions.size();
		_functions.push_back({name, friendlyName, toolTip, image, params, false, false, operatorBarOnly, logicalSuffix});
	};

	auto P = [](const std::string & name, const stringvec & keys) { return ScriptParamDef::fromRaw(name, keys); };

	static const stringvec numKeys		= {"number"},
							boolKeys	= {"boolean"},
							strKeys		= {"string"},
							boolStrNum	= {"boolean", "string", "number"},
							strNum		= {"string", "number"},
							strBoolNum	= {"string", "boolean", "number"};

	addFunc("abs",		"", tr("absolute value"),					{P("values", numKeys)});
	addFunc("sd",		"", tr("standard deviation"),				{P("values", numKeys)},	"sigma.png");
	addFunc("var",		"", tr("variance"),						{P("values", numKeys)},	"variance.png");
	addFunc("sum",		"", tr("summation"),						{P("values", numKeys)},	"sum.png");
	addFunc("prod",		"", tr("product of values"),				{P("values", numKeys)},	"product.png");
	addFunc("zScores",	"", tr("Standardizes the variable"),		{P("values", numKeys)});
	addFunc("min",		"", tr("returns minimum of values"),		{P("values", numKeys)});
	addFunc("max",		"", tr("returns maximum of values"),		{P("values", numKeys)});
	addFunc("mean",		"", tr("mean"),							{P("values", numKeys)});
	addFunc("sign",		"", tr("returns the sign of values"),		{P("values", numKeys)});
	addFunc("round",	"", tr("rounds y to n decimals"),			{P("y", numKeys), P("n", numKeys)});
	addFunc("length",	"", tr("returns number of elements in y"),	{P("y", strNum)});
	addFunc("median",	"", tr("median"),							{P("values", numKeys)});
	addFunc("ifelse",	"", tr("if-else statement"),				{P("test", boolKeys), P("then", boolStrNum), P("else", boolStrNum)});
	addFunc("hasSubstring", "", tr("returns true if string contains substring at least once"), {P("string", strKeys), P("substring", strKeys)});
	addFunc("is.na",	"", tr("Combine with not-operator to filter out rows with missing values (NA) for a column."), {P("y", strBoolNum)});

	// sqrt and ! live only in the operator bar (interspersed with the operators), not in the
	// right-hand function palette.
	addFunc("sqrt",		"", tr("Square root"),						{P("value(s)", numKeys)},	"rootHead.png", true);
	addFunc("!",		"", tr("Not: %1"),							{P("logical(s)", boolKeys)},	"negative.png", true, true);

	addFunc("log",		"", tr("natural logarithm"),				{P("y", numKeys)});
	addFunc("log2",		"log\u2082", tr("base 2 logarithm"),		{P("y", numKeys)});
	addFunc("log10",	"log\u2081\u2080", tr("base 10 logarithm"),	{P("y", numKeys)});
	addFunc("logb",		"", tr("logarithm of y in 'base'"),		{P("y", numKeys), P("base", numKeys)});
	addFunc("exp",		"", tr("exponential"),						{P("y", numKeys)});
	addFunc("fishZ",	"", tr("Fisher's Z-transform (i.e., the inverse hyperbolic tangent) to transform correlations, numbers between -1 and 1 to the real line"), {P("y", numKeys)});
	addFunc("invFishZ",	"fishZ\u207B\u00B9", tr("Inverse Fisher's Z-transform (i.e., the hyperbolic tangent) to transform real numbers to numbers between -1 and 1"), {P("y", numKeys)});
	addFunc("logit",	"", tr("Logit transform (i.e., the inverse of the standard logit function, or log-odds transform) converts numbers between 0 and 1 to the real line."), {P("y", numKeys)});
	addFunc("invLogit",	"logit\u207B\u00B9", tr("Inverse logit transform (i.e., the standard logit function) converts numbers on the real line to numbers between 0 and 1."), {P("y", numKeys)});
	addFunc("BoxCox",	"", tr("Two-parameter Box-Cox transform (transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like."), {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)});
	addFunc("BoxCoxAuto", "", tr("Two-parameter Box-Cox transform with an automatic determination of the shape parameter lambda, according to one of the three of methods:'loglik', 'sd', or 'movingRange'. The search for optimal lambda is bounded within 'lower' and 'upper' limits."), {P("y", numKeys), P("?predictor", numKeys), P("?groupSize", numKeys), P("method", strKeys), P("lower", numKeys), P("upper", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)});
	addFunc("invBoxCox", "BoxCox\u207B\u00B9", tr("Inverse two-parameter Box-Cox transform."), {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)});
	addFunc("powerTransform", "", tr("Two-parameter power transform (scale-invariant Box-Box; transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like."), {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys)});
	addFunc("powerTransformAuto", "", tr("Two-parameter power transform with an automatic determination of the shape parameter lambda. The search for optimal lambda is bounded within 'lower' and 'upper' limits."), {P("y", numKeys), P("?predictor", numKeys), P("?groupSize", numKeys), P("lower", numKeys), P("upper", numKeys), P("shift", numKeys)});
	addFunc("YeoJohnson", "", tr("Yeo-Johnson transform (transforms any real values) to stabilize variance and attempt to make the data more normal distribution-like."), {P("y", numKeys), P("lambda", numKeys)});
	addFunc("YeoJohnsonAuto", "", tr("Yeo-Johnson transform (transforms any real values) with an automatic determination of the shape parameter lambda. The search for optimal lambda is bounded within 'lower' and 'upper' limits."), {P("y", numKeys), P("lower", numKeys), P("upper", numKeys)});
	addFunc("Johnson",	"", tr("Johnson transform (transforms any real values). The search for optimal parameter is bounded within 'lower' and 'upper' limits."), {P("y", numKeys), P("lower", numKeys), P("upper", numKeys)});

	addFunc("cut",		"", tr("break your data up in numBreaks levels"), {P("values", numKeys), P("numBreaks", numKeys)});
	addFunc("replaceNA", "", tr("replace any missing values (NA) in column by the value in replaceWith"), {P("column", strBoolNum), P("replaceWith", strBoolNum)});
	addFunc("ifElse",	"", tr("if-else statement"),				{P("test", boolKeys), P("then", boolStrNum), P("else", boolStrNum)});

	addFunc("normalDist",	"", tr("generates data from a Gaussian distribution with specified mean and standard deviation sd"), {P("mean", numKeys), P("sd", numKeys)});
	addFunc("tDist",		"", tr("generates data from t distribution with degrees of freedom df and non-centrality parameter ncp"), {P("df", numKeys), P("ncp", numKeys)});
	addFunc("chiSqDist",	"", tr("generates data from a chi-squared distribution with degrees of freedom df and non-centrality parameter ncp"), {P("df", numKeys), P("ncp", numKeys)});
	addFunc("fDist",		"", tr("generates data from an F distribution with specified degrees of freedoms df1, df2 and non-centrality parameter ncp"), {P("df1", numKeys), P("df2", numKeys), P("ncp", numKeys)});
	addFunc("binomDist",	"", tr("generates data from a binomial distribution with specified trials and probability prob"), {P("trials", numKeys), P("prob", numKeys)});
	addFunc("negBinomDist",	"", tr("generates data from a negative binomial distribution with specified trials and probability prob"), {P("targetTrial", numKeys), P("prob", numKeys)});
	addFunc("geomDist",		"", tr("generates data from a geometric distribution with specified probability prob"), {P("prob", numKeys)});
	addFunc("poisDist",		"", tr("generates data from a Poisson distribution with specified rate lambda"), {P("lambda", numKeys)});
	addFunc("betaDist",		"", tr("generates data from a beta distribution with specified shapes alpha and beta"), {P("alpha", numKeys), P("beta", numKeys)});
	addFunc("unifDist",		"", tr("generates data from a uniform distribution between min and max"), {P("min", numKeys), P("max", numKeys)});
	addFunc("gammaDist",	"", tr("generates data from a gamma distribution with specified shape and scale"), {P("shape", numKeys), P("scale", numKeys)});
	addFunc("expDist",		"", tr("generates data from an exponential distribution with specified rate"), {P("rate", numKeys)});
	addFunc("logNormDist",	"", tr("generates data from a log-normal distribution with specified logarithmic mean meanLog and standard deviation sdLog"), {P("meanLog", numKeys), P("sdLog", numKeys)});
	addFunc("weibullDist",	"", tr("generates data from a Weibull distribution with specified shape and scale"), {P("shape", numKeys), P("scale", numKeys)});

	auto addRowFunc = [this](const std::string & name, const QString & toolTip, const std::string & image = "")
	{
		_rowFunctionIndex[name] = _rowFunctions.size();
		_rowFunctions.push_back({name, name, toolTip, image, {}, true, true, false, false});
	};

	addRowFunc("rowMean",		tr("Rowwise mean"));
	addRowFunc("rowSum",		tr("Rowwise sum"),					"sum.png");
	addRowFunc("rowSD",			tr("Rowwise standard deviation"),	"sigma.png");
	addRowFunc("rowVariance",	tr("Rowwise variance"),				"variance.png");
	addRowFunc("rowMedian",		tr("Rowwise median"));
	addRowFunc("rowMin",		tr("Rowwise minimum"));
	addRowFunc("rowMax",		tr("Rowwise maximum"));
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
		if(def.operatorBarOnly)												continue;
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
