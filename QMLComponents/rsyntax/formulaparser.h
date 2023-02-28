#ifndef FORMULAPARSER_H
#define FORMULAPARSER_H

#include <QCoreApplication>
#include "models/terms.h"

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

	static bool			parse(const Json::Value& formula, bool isLhs, ParsedTerms& parsedTerms, QString& error);
	static Terms		parseTerm(QString term);
	static Terms		parseTerm(const Json::Value& jsonString);
	static Terms		parseTerms(const Json::Value& json);
//	static ParsedTerm	parseTerm(const QString& term, const ParsedTerms& conditionalParsedTerms, bool isCorrelated);
//	static ParsedTerm	parseTerm(const ParsedTerm& term, const ParsedTerms& conditionalParsedTerms, bool isCorrelated);

	static QString		transformToFormulaTerm(const Term& term, char join = FormulaParser::allInterationsSeparator, bool withCrossCombinations = false);

private:
	static ParsedTerms	squeezeConditionalTerms(const ParsedTerms& terms);
};

#endif // FORMULAPARSER_H
