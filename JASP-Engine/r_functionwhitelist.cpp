#include "r_functionwhitelist.h"

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
	"asin",
	"atan",
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
	"for",
	"format",
	"function",
	"gl",
	"glm",
	"grep",
	"gsub",
	"if",
	"ifelse",
	"intersect",
	"is.array",
	"is.character",
	"is.complex",
	"is.data.frame",
	"is.element",
	"is.na",
	"is.null",
	"is.numeric",
	"lapply",
	"length",
	"library",
	"list",
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
	"rep",
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
	"stack",
	"str",
	"strsplit",
	"subset",
	"substr",
	"sum",
	"summary",
	"t",
	"t.test",
	"table",
	"tan",
	"tapply",
	"tolower",
	"toupper",
	"unclass",
	"union",
	"unique",
	"unstack",
	"var",
	"weighted.mean",
	"which",
	"which.max",
	"which.min",
	"xtabs"
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
const std::string	R_FunctionWhiteList::functionNameBody("(?:\\w|\\.)+");
const std::regex	R_FunctionWhiteList::functionNameMatcher(functionStartDelimit + "(" + functionNameStart + functionNameBody + ")(?=\\s*\\()");

std::set<std::string> R_FunctionWhiteList::scriptIsSafe(std::string const & script)
{
	//std::cout << "entered scriptIsSafe with " << script << std::endl << std::flush;

	std::set<std::string> blackListedFunctionsFound;

	auto foundFunctionsBegin	= std::sregex_iterator(script.begin(), script.end(), functionNameMatcher);
	auto foundFunctionsEnd		= std::sregex_iterator();

	for(auto foundFunctionIter = foundFunctionsBegin; foundFunctionIter != foundFunctionsEnd; foundFunctionIter++ )
	{
		std::string foundFunction((*foundFunctionIter)[1].str());
		bool whiteListed = functionWhiteList.count(foundFunction) > 0;
		//std::cout << "I found " << (whiteListed ? "white" : "black") << "-listed function: " << foundFunction << std::endl;

		if(!whiteListed && blackListedFunctionsFound.count(foundFunction) == 0)
			blackListedFunctionsFound.insert(foundFunction);
	}
	std::cout << std::flush;

	return blackListedFunctionsFound;
}
