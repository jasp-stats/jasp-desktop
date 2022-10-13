#ifndef FORMULAPARSER_H
#define FORMULAPARSER_H

#include <QCoreApplication>
#include "models/terms.h"

class FormulaParser
{
	Q_DECLARE_TR_FUNCTIONS(FormulaParser)
public:
	struct ParsedTerm
	{
		Terms		baseTerms,
					allTerms,
					conditionalTerms;
		bool		isConditional = false,
					isCorrelated = false,
					hasInteraction = false;
		ParsedTerm() {}
	};

	typedef QVector<ParsedTerm> ParsedTerms;

	static const char interactionSeparator;
	static const char allInterationsSeparator;

	static bool			parse(const QString& formula, ParsedTerms& parsedTerms, QString& error);
	static ParsedTerm	parseTerm(const QString& term);
	static ParsedTerm	parseTerm(const QString& term, const ParsedTerms& conditionalParsedTerms, bool isCorrelated);
	static ParsedTerm	parseTerm(const ParsedTerm& term, const ParsedTerms& conditionalParsedTerms, bool isCorrelated);

	static QString		transformToFormulaTerm(const Term& term, char join = FormulaParser::allInterationsSeparator, bool withCrossCombinations = false);

private:
	static ParsedTerms	squeezeConditionalTerms(const ParsedTerms& terms);
};

#endif // FORMULAPARSER_H
