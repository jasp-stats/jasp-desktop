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
	const QString translated = QCoreApplication::translate("ScriptConstructorRegistry", toolTip.toUtf8().constData());

	if(!logicalSuffix)
		return translated;

	return translated.arg(mode == ScriptConstructorMode::Filter
		? QCoreApplication::translate("ScriptConstructorRegistry", "returns logicals and can be the root of a filter formula")
		: QCoreApplication::translate("ScriptConstructorRegistry", "returns logicals"));
}

QString ScriptFunctionDef::toolTipForMode(ScriptConstructorMode mode) const
{
	const QString translated = QCoreApplication::translate("ScriptConstructorRegistry", toolTip.toUtf8().constData());

	if(!logicalSuffix)
		return translated;

	return translated.arg(mode == ScriptConstructorMode::Filter
		? QCoreApplication::translate("ScriptConstructorRegistry", "returns logicals and can be the root of a filter formula")
		: QCoreApplication::translate("ScriptConstructorRegistry", "returns logicals"));
}

ScriptConstructorRegistry::ScriptConstructorRegistry()
{
	auto addOp = [this](const std::string & op, const QString & toolTip, const std::string & image = "", bool vertical = false, bool logicalSuffix = false)
	{
		_operatorIndex[op + (vertical ? "V" : "")] = _operators.size();
		_operators.push_back({op, toolTip, image, vertical, logicalSuffix});
	};

	addOp("+",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Addition"),								"plus.png");
	addOp("-",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Subtraction"),							"minus.png");
	addOp("*",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Multiplication"),						"multiply.png");
	addOp("/",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Division"),								"divide.png", true);
	addOp("/",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Division"),								"");
	addOp("^",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Power (2^3 returns 8)"),					"");
	addOp("%%",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Modulo: returns the remainder of a division. 3%2 returns 1"),	"modulo.png");
	addOp("==",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Equality: %1"),							"equal.png", false, true);
	addOp("!=",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Inequality: %1"),						"notEqual.png", false, true);
	addOp("<",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Less than: %1"),						"lessThan.png", false, true);
	addOp("<=",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Less than or equal to: %1"),			"lessThanEqual.png", false, true);
	addOp(">",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Greater than: %1"),						"greaterThan.png", false, true);
	addOp(">=",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Greater than or equal to: %1"),			"greaterThanEqual.png", false, true);
	addOp("&",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "And: %1"),								"and.png", false, true);
	addOp("|",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Or: %1"),								"or.png", false, true);
	addOp("%|%",	QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Split: applies filter separately to each subgroup"),	"ConditionBy.png");

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

	addFunc("abs",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "absolute value"),					{P("values", numKeys)});
	addFunc("sd",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "standard deviation"),				{P("values", numKeys)},	"sigma.png");
	addFunc("var",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "variance"),						{P("values", numKeys)},	"variance.png");
	addFunc("sum",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "summation"),						{P("values", numKeys)},	"sum.png");
	addFunc("prod",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "product of values"),				{P("values", numKeys)},	"product.png");
	addFunc("zScores",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Standardizes the variable"),		{P("values", numKeys)});
	addFunc("min",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns minimum of values"),		{P("values", numKeys)});
	addFunc("max",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns maximum of values"),		{P("values", numKeys)});
	addFunc("mean",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "mean"),							{P("values", numKeys)});
	addFunc("sign",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns the sign of values"),		{P("values", numKeys)});
	addFunc("round",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "rounds y to n decimals"),			{P("y", numKeys), P("n", numKeys)});
	addFunc("length",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns number of elements in y"),	{P("y", strNum)});
	addFunc("median",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "median"),							{P("values", numKeys)});
	addFunc("ifelse",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "if-else statement"),				{P("test", boolKeys), P("then", boolStrNum), P("else", boolStrNum)});
	addFunc("hasSubstring", "", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "returns true if string contains substring at least once"), {P("string", strKeys), P("substring", strKeys)});
	addFunc("is.na",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Combine with not-operator to filter out rows with missing values (NA) for a column."), {P("y", strBoolNum)});

	// sqrt and ! live only in the operator bar (interspersed with the operators), not in the
	// right-hand function palette.
	addFunc("sqrt",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Square root"),						{P("value(s)", numKeys)},	"rootHead.png", true);
	addFunc("!",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Not: %1"),							{P("logical(s)", boolKeys)},	"negative.png", true, true);

	addFunc("log",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "natural logarithm"),				{P("y", numKeys)});
	addFunc("log2",		"log\u2082", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "base 2 logarithm"),		{P("y", numKeys)});
	addFunc("log10",	"log\u2081\u2080", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "base 10 logarithm"),	{P("y", numKeys)});
	addFunc("logb",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "logarithm of y in 'base'"),		{P("y", numKeys), P("base", numKeys)});
	addFunc("exp",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "exponential"),						{P("y", numKeys)});
	addFunc("fishZ",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Fisher's Z-transform (i.e., the inverse hyperbolic tangent) to transform correlations, numbers between -1 and 1 to the real line"), {P("y", numKeys)});
	addFunc("invFishZ",	"fishZ\u207B\u00B9", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Inverse Fisher's Z-transform (i.e., the hyperbolic tangent) to transform real numbers to numbers between -1 and 1"), {P("y", numKeys)});
	addFunc("logit",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Logit transform (i.e., the inverse of the standard logit function, or log-odds transform) converts numbers between 0 and 1 to the real line."), {P("y", numKeys)});
	addFunc("invLogit",	"logit\u207B\u00B9", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Inverse logit transform (i.e., the standard logit function) converts numbers on the real line to numbers between 0 and 1."), {P("y", numKeys)});
	addFunc("BoxCox",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Two-parameter Box-Cox transform (transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like."), {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)});
	addFunc("BoxCoxAuto", "", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Two-parameter Box-Cox transform with an automatic determination of the shape parameter lambda, according to one of the three of methods:'loglik', 'sd', or 'movingRange'. The search for optimal lambda is bounded within 'lower' and 'upper' limits."), {P("y", numKeys), P("?predictor", numKeys), P("?groupSize", numKeys), P("method", strKeys), P("lower", numKeys), P("upper", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)});
	addFunc("invBoxCox", "BoxCox\u207B\u00B9", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Inverse two-parameter Box-Cox transform."), {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys), P("continuityAdjustment", boolKeys)});
	addFunc("powerTransform", "", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Two-parameter power transform (scale-invariant Box-Box; transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like."), {P("y", numKeys), P("lambda", numKeys), P("shift", numKeys)});
	addFunc("powerTransformAuto", "", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Two-parameter power transform with an automatic determination of the shape parameter lambda. The search for optimal lambda is bounded within 'lower' and 'upper' limits."), {P("y", numKeys), P("?predictor", numKeys), P("?groupSize", numKeys), P("lower", numKeys), P("upper", numKeys), P("shift", numKeys)});
	addFunc("YeoJohnson", "", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Yeo-Johnson transform (transforms any real values) to stabilize variance and attempt to make the data more normal distribution-like."), {P("y", numKeys), P("lambda", numKeys)});
	addFunc("YeoJohnsonAuto", "", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Yeo-Johnson transform (transforms any real values) with an automatic determination of the shape parameter lambda. The search for optimal lambda is bounded within 'lower' and 'upper' limits."), {P("y", numKeys), P("lower", numKeys), P("upper", numKeys)});
	addFunc("Johnson",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Johnson transform (transforms any real values). The search for optimal parameter is bounded within 'lower' and 'upper' limits."), {P("y", numKeys), P("lower", numKeys), P("upper", numKeys)});

	addFunc("cut",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "break your data up in numBreaks levels"), {P("values", numKeys), P("numBreaks", numKeys)});
	addFunc("replaceNA", "", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "replace any missing values (NA) in column by the value in replaceWith"), {P("column", strBoolNum), P("replaceWith", strBoolNum)});
	addFunc("ifElse",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "if-else statement"),				{P("test", boolKeys), P("then", boolStrNum), P("else", boolStrNum)});

	addFunc("normalDist",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a Gaussian distribution with specified mean and standard deviation sd"), {P("mean", numKeys), P("sd", numKeys)});
	addFunc("tDist",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from t distribution with degrees of freedom df and non-centrality parameter ncp"), {P("df", numKeys), P("ncp", numKeys)});
	addFunc("chiSqDist",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a chi-squared distribution with degrees of freedom df and non-centrality parameter ncp"), {P("df", numKeys), P("ncp", numKeys)});
	addFunc("fDist",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from an F distribution with specified degrees of freedoms df1, df2 and non-centrality parameter ncp"), {P("df1", numKeys), P("df2", numKeys), P("ncp", numKeys)});
	addFunc("binomDist",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a binomial distribution with specified trials and probability prob"), {P("trials", numKeys), P("prob", numKeys)});
	addFunc("negBinomDist",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a negative binomial distribution with specified trials and probability prob"), {P("targetTrial", numKeys), P("prob", numKeys)});
	addFunc("geomDist",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a geometric distribution with specified probability prob"), {P("prob", numKeys)});
	addFunc("poisDist",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a Poisson distribution with specified rate lambda"), {P("lambda", numKeys)});
	addFunc("betaDist",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a beta distribution with specified shapes alpha and beta"), {P("alpha", numKeys), P("beta", numKeys)});
	addFunc("unifDist",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a uniform distribution between min and max"), {P("min", numKeys), P("max", numKeys)});
	addFunc("gammaDist",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a gamma distribution with specified shape and scale"), {P("shape", numKeys), P("scale", numKeys)});
	addFunc("expDist",		"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from an exponential distribution with specified rate"), {P("rate", numKeys)});
	addFunc("logNormDist",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a log-normal distribution with specified logarithmic mean meanLog and standard deviation sdLog"), {P("meanLog", numKeys), P("sdLog", numKeys)});
	addFunc("weibullDist",	"", QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "generates data from a Weibull distribution with specified shape and scale"), {P("shape", numKeys), P("scale", numKeys)});

	auto addRowFunc = [this](const std::string & name, const QString & toolTip, const std::string & image = "")
	{
		_rowFunctionIndex[name] = _rowFunctions.size();
		_rowFunctions.push_back({name, name, toolTip, image, {}, true, true, false, false});
	};

	addRowFunc("rowMean",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise mean"));
	addRowFunc("rowSum",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise sum"),					"sum.png");
	addRowFunc("rowSD",			QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise standard deviation"),	"sigma.png");
	addRowFunc("rowVariance",	QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise variance"),				"variance.png");
	addRowFunc("rowMedian",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise median"));
	addRowFunc("rowMin",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise minimum"));
	addRowFunc("rowMax",		QT_TRANSLATE_NOOP("ScriptConstructorRegistry", "Rowwise maximum"));
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
