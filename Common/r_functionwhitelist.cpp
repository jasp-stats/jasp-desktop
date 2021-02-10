#include "r_functionwhitelist.h"
#include "stringutils.h"

	//The following functions (and keywords that can be followed by a '(') will be allowed in user-entered R-code, such as filters or computed columns. This is for security because otherwise JASP-files could become a vector of attack and that doesn't refer to an R-datatype.
const std::set<std::string> R_FunctionWhiteList::functionWhiteList {
	"AIC",
	"Arg",
	"Conj",
	"Im",
	"Mod",
	"NCOL",
	"Re",
	"abs",
	"acos",
	"aggregate",
	"anova",
	"aov",
	"apply",
	"approx",
	"array",
	"as.Date",
	"as.POSIXct",
	"as.array",
	"as.character",
	"as.complex",
	"as.data.frame",
	"as.logical",
	"as.numeric",
	"as.factor",
	"as.list",
	"as.integer",
	"asin",
	"atan",
	"atanh",
	"atan2",
	"attr",
	"attributes",
	"binom.test",
	"by",
	"c",
	"cat",
	"cbind",
	"choose",
	"class",
	"coef",
	"colMeans",
	"colSums",
	"colsum",
	"convolve",
	"cor",
	"cos",
	"cummax",
	"cummin",
	"cumprod",
	"cumsum",
	"cut",
	"data.frame",
	"density",
	"deviance",
	"df.residual",
	"diag",
	"diff",
	"dim",
	"dimnames",
	"exp",
	"expand.grid",
	"factor",
	"fft",
	"filter",
	"fitted",
	"fishZ",
	"for",
	"format",
	"function",
	"gl",
	"glm",
	"gregexpr",
	"grep",
	"grepl",
	"gsub",
	"if",
	"ifelse",
	"ifElse",
	"intersect",
	"invFishZ",
	"is.array",
	"is.character",
	"is.complex",
	"is.data.frame",
	"is.element",
	"is.logical",
	"is.na",
	"is.null",
	"is.numeric",
	"lag",
	"lapply",
	"length",
	"library",
	"list",
	"local",
	"lm",
	"loess",
	"log",
	"log10",
	"logLik",
	"ls",
	"ls.srt",
	"match",
	"matrix",
	"max",
	"mean",
	"median",
	"merge",
	"methods",
	"min",
	"mvfft",
	"na.fail",
	"na.omit",
	"nchar",
	"ncol",
	"nlm",
	"nls",
	"nrow",
	"optim",
	"pairwise.t.test",
	"paste",
	"paste0",
	"pmatch",
	"pmax",
	"pmin",
	"power.t.test",
	"predict",
	"print",
	"prod",
	"prop.table",
	"prop.test",
	"quantile",
	"range",
	"rank",
	"rbeta",
	"rbind",
	"rbinom",
	"rcauchy",
	"rchisq",
	"regexec",
	"regexpr",
	"rep",
	"replicate",
	"reshape",
	"residuals",
	"return",
	"rev",
	"rexp",
	"rf",
	"rgamma",
	"rgeom",
	"rhyper",
	"rlnorm",
	"rlogis",
	"rnbinom",
	"rnorm",
	"round",
	"rowMeans",
	"rowSums",
	"rowsum",
	"rpois",
	"rt",
	"runif",
	"rweibull",
	"rwilcox",
	"sample",
	"scale",
	"sd",
	"seq",
	"setdiff",
	"setequal",
	"sin",
	"solve",
	"sort",
	"spline",
	"sqrt",
	"stack",
	"str",
	"strsplit",
	"sub",
	"subset",
	"substr",
	"sum",
	"summary",
	"t",
	"t.test",
	"table",
	"tan",
	"tanh",
	"tapply",
	"tolower",
	"toString",
	"toupper",
	"trimws",
	"unclass",
	"union",
	"unique",
	"unstack",
	"var",
	"weighted.mean",
	"which",
	"which.max",
	"which.min",
	"xtabs",
	".setColumnDataAsScale", ".setColumnDataAsOrdinal", ".setColumnDataAsNominal", ".setColumnDataAsNominalText", "function", "stop",
	"normalDist", "tDist", "chiSqDist", "fDist", "binomDist", "negBinomDist", "geomDist", "poisDist", "integerDist", "betaDist", "unifDist", "gammaDist", "expDist", "logNormDist", "weibullDist",
	"replaceNA",
	//Some distribution related stuff:
	"dbeta",		"pbeta",		"qbeta",		"rbeta",
	"dbinom",		"pbinom",		"qbinom",		"rbinom",
	"dcauchy",		"pcauchy",		"qcauchy",		"rcauchy",
	"dchisq",		"pchisq",		"qchisq",		"rchisq",
	"dexp",			"pexp",			"qexp",			"rexp",
	"df",			"pf",			"qf",			"rf",
	"dgamma",		"pgamma",		"qgamma",		"rgamma",
	"dgeom",		"pgeom",		"qgeom",		"rgeom",
	"dhyper",		"phyper",		"qhyper",		"rhyper",
	"dlnorm",		"plnorm",		"qlnorm",		"rlnorm",
	"dmultinom",	"pmultinom",	"qmultinom",	"rmultinom",
	"dnbinom",		"pnbinom",		"qnbinom",		"rnbinom",
	"dnorm",		"pnorm",		"qnorm",		"rnorm",
	"dpois",		"ppois",		"qpois",		"rpois",
	"dt",			"pt",			"qt",			"rt",
	"dunif",		"punif",		"qunif",		"runif",
	"dweibull",		"pweibull",		"qweibull",		"rweibull",
	"dsignrank",	"psignrank",	"qsignrank",	"rsignrank",

	"pbirthday",
	"ptukey",
	"dwilcox",
	"switch"
#ifdef JASP_DEBUG
	,"Sys.sleep", ".crashPlease", "stringi::stri_enc_mark", "stringi::stri_enc_toutf8", "Encoding"
#endif
	};

std::string R_FunctionWhiteList::returnOrderedWhiteList()
{
	std::stringstream out;

	for(auto & s : functionWhiteList)
		out << "\"" << s << "\"," << std::endl;
	out << std::flush;

	return out.str();
}

const std::string	R_FunctionWhiteList::functionStartDelimit("(?:[;\\s\\(\"\\[\\+\\-\\=\\*\\%\\/\\{\\|&!]|^)"); //These should be all possible non-funtion-name-characters that could be right in front of any function-name in R.
const std::string	R_FunctionWhiteList::functionNameStart("(?:\\.?[[:alpha:]])");
const std::string	R_FunctionWhiteList::functionNameBody("(?:\\w|\\.|::)+");
const std::regex	R_FunctionWhiteList::functionNameMatcher(functionStartDelimit + "(" + functionNameStart + functionNameBody + ")(?=[\\t \\r]*\\()");

std::set<std::string> R_FunctionWhiteList::findIllegalFunctions(std::string const & script)
{
	std::set<std::string> blackListedFunctionsFound;

	auto foundFunctionsBegin	= std::sregex_iterator(script.begin(), script.end(), functionNameMatcher);
	auto foundFunctionsEnd		= std::sregex_iterator();

	for(auto foundFunctionIter = foundFunctionsBegin; foundFunctionIter != foundFunctionsEnd; foundFunctionIter++ )
	{
		std::string foundFunction((*foundFunctionIter)[1].str());
		bool whiteListed = functionWhiteList.count(foundFunction) > 0;

		if(!whiteListed && blackListedFunctionsFound.count(foundFunction) == 0)
			blackListedFunctionsFound.insert(foundFunction);
	}


	return blackListedFunctionsFound;
}

const std::string	R_FunctionWhiteList::operatorsR("`(?:\\+|-|\\*|/|%(?:/|\\*|in)?%|\\^|<=?|>=?|==?|!=?|<?<-|->>?|\\|\\|?|&&?|:|\\$)`");

const std::regex	R_FunctionWhiteList::assignmentWhiteListedRightMatcher(	"(" +				functionNameStart + functionNameBody +	")\\s*(?:<?<-|=)");
const std::regex	R_FunctionWhiteList::assignmentWhiteListedLeftMatcher(	"(?:->>?)\\s*(" +	functionNameStart + functionNameBody +	")");
const std::regex	R_FunctionWhiteList::assignmentOperatorRightMatcher(	"(" +				operatorsR +							")\\s*(?:<?<-|=)");
const std::regex	R_FunctionWhiteList::assignmentOperatorLeftMatcher(		"(?:->>?)\\s*(" +	operatorsR +							")");

std::set<std::string> R_FunctionWhiteList::findIllegalFunctionsAliases(std::string const & script)
{
	//Log::log() << "findIllegalFunctionsAliases with " << script << std::endl;

	std::set<std::string> illegalAliasesFound;

	auto aliasSearcher = [&illegalAliasesFound, &script](std::regex aliasAssignmentMatcher, bool (*lambdaMatchChecker)(std::string) )
	{
		auto foundAliasesBegin	= std::sregex_iterator(script.begin(), script.end(), aliasAssignmentMatcher);
		auto foundAliasesEnd	= std::sregex_iterator();

		for(auto foundAliasesIter = foundAliasesBegin; foundAliasesIter != foundAliasesEnd; foundAliasesIter++ )
		{
			std::string foundAlias((*foundAliasesIter)[1].str());
			bool allowed = (*lambdaMatchChecker)(foundAlias);

			//Log::log() << "I found alias assignment: " << foundAlias << " which is " << (allowed ? "allowed" : "not allowed") << ".." << std::endl;

			if(!allowed)
				illegalAliasesFound.insert(foundAlias);
		}
	};

	aliasSearcher(assignmentOperatorLeftMatcher,		[](std::string alias) { return false; }); //operators are never allowed
	aliasSearcher(assignmentOperatorRightMatcher,		[](std::string alias) { return false; });
	aliasSearcher(assignmentWhiteListedLeftMatcher,		[](std::string alias) { return functionWhiteList.count(alias) == 0; }); //only allowed when the token being assigned to is not in whitelist
	aliasSearcher(assignmentWhiteListedRightMatcher,	[](std::string alias) { return functionWhiteList.count(alias) == 0; });

	//Log::log() << std::flush;

	return illegalAliasesFound;
}

void R_FunctionWhiteList::scriptIsSafe(const std::string &script)
{
    std::string commentFree = stringUtils::stripRComments(script);

	static std::string errorMsg;

	std::set<std::string> blackListedFunctions = findIllegalFunctions(commentFree);

	if(blackListedFunctions.size() > 0)
	{
		bool moreThanOne = blackListedFunctions.size() > 1;
		std::stringstream ssm;
		ssm << "Non-whitelisted function" << (moreThanOne ? "s" : "") << " used:" << (moreThanOne ? "\n" : " ");
		for(auto & black : blackListedFunctions)
			ssm << black << "\n";
		errorMsg = ssm.str();

		throw filterException(errorMsg);
	}

	std::set<std::string> illegalAliasesFound = findIllegalFunctionsAliases(commentFree);

	if(illegalAliasesFound.size() > 0)
	{
		bool moreThanOne = illegalAliasesFound.size() > 1;
		std::stringstream ssm;
		ssm << "Illegal assignment to " << (moreThanOne ? "operators or whitelisted functions" : "an operator or whitelisted function") << " used:" << (moreThanOne ? "\n" : " ");
		for(auto & alias : illegalAliasesFound)
			ssm << alias << "\n";
		errorMsg = ssm.str();

		throw filterException(errorMsg);
	}
}
