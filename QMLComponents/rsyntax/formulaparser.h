#ifndef FORMULAPARSER_H
#define FORMULAPARSER_H

#include <QCoreApplication>
#include "models/terms.h"

class VariableInfoProvider;

class FormulaParser
{
	Q_DECLARE_TR_FUNCTIONS(FormulaParser)
public:
	struct RandomTerm
	{
		Terms	terms;
		bool	intercept = true,
				correlated = true;

		RandomTerm() {}

	};
	struct ParsedTerms
	{
		Terms		fixedTerms;
		bool		intercept = true;
		QMap<QString, RandomTerm>	randomTerms;

		ParsedTerms() {}
	};

	static const char interactionSeparator;
	static const char allInterationsSeparator;

	static bool			parse(const Json::Value& formula, bool isLhs, ParsedTerms& parsedTerms, QString& error, VariableInfoProvider * provider);
	static Terms		parseTerm(QString term,																	VariableInfoProvider * provider);
	static Terms		parseTerm(const Json::Value& jsonString,												VariableInfoProvider * provider);
	static Terms		parseTerms(const Json::Value& json,														VariableInfoProvider * provider);
//	static ParsedTerm	parseTerm(const QString& term, const ParsedTerms& conditionalParsedTerms, bool isCorrelated);
//	static ParsedTerm	parseTerm(const ParsedTerm& term, const ParsedTerms& conditionalParsedTerms, bool isCorrelated);

	// `contextFilterId` is the default namespace bare names resolve in at run
	// time (the analysis' own filter). A term stamped with a DIFFERENT live filter id renders
	// as the D7.0a qualified MINIMAL form (renderQualifiedColumnRef: bare inside the context,
	// "DS"."Col" / "F"."Col" / full form outside it) - qualification never happens for -1
	// (unqualified) terms, so every legacy emission stays byte-identical. The default -1 keeps
	// every caller that cannot know a context on the legacy path.
	static QString		transformToFormulaTerm(const Term& term, const Json::Value& changedType = Json::nullValue, char join = FormulaParser::allInterationsSeparator, bool withCrossCombinations = false, int contextFilterId = -1);

private:
	static ParsedTerms	squeezeConditionalTerms(const ParsedTerms& terms);
};

#endif // FORMULAPARSER_H
