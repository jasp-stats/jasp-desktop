#include "formulaparser.h"
#include <QRegularExpression>

const char FormulaParser::interactionSeparator			= ':';
const char FormulaParser::allInterationsSeparator		= '*';

FormulaParser::ParsedTerm FormulaParser::parseTerm(const QString &term)
{
	ParsedTerm parsedTerm;

	auto trim = [] (const QString& input) -> QString
	{
		QString output = input.trimmed();
		if (output.startsWith('`'))		output = output.mid(1);
		if (output.endsWith('`'))		output = output.chopped(1);

		return output;
	};

	auto trimList = [=] (const QString& input, const char& separator) -> QStringList
	{
		QStringList output;
		if (input.contains(separator))
		{
			QStringList components = input.split(separator);
			for (const QString& component : components)
				output.append(trim(component));
		}
		else
			output.append(trim(input));

		return output;
	};

	// One term in a formula, can be a, a:b, or a*b.
	// a*b corresponds to a, b & a:b, so we have several terms, that's why this function returns Terms in place of Term
	if (term.contains(allInterationsSeparator))
	{
		parsedTerm.baseTerms = trimList(term, allInterationsSeparator);
		parsedTerm.allTerms = parsedTerm.baseTerms.crossCombinations();
		parsedTerm.hasInteraction = true;
	}
	else if (term.contains(interactionSeparator))
	{
		parsedTerm.baseTerms = trimList(term, interactionSeparator);
		// The Term class does not use ':' for the interaction, but a '*' (a bit confusing...)
		QString aterm = term;
		parsedTerm.allTerms.add(Term::readTerm(aterm.replace(interactionSeparator, Term::separator)));
		parsedTerm.hasInteraction = true;
	}
	else
	{
		parsedTerm.baseTerms.add(trim(term));
		parsedTerm.allTerms = parsedTerm.baseTerms;
		parsedTerm.hasInteraction = false;
	}

	return parsedTerm;
}

FormulaParser::ParsedTerm FormulaParser::parseTerm(const QString& term, const ParsedTerms& conditionalParsedTerms, bool isCorrelated)
{
	return parseTerm(parseTerm(term), conditionalParsedTerms, isCorrelated);
}

FormulaParser::ParsedTerm FormulaParser::parseTerm(const ParsedTerm& term, const ParsedTerms& conditionalParsedTerms, bool isCorrelated)
{
	ParsedTerm parsedTerm = term;
	Terms conditionalTerms;
	for (const ParsedTerm& conditionalParsedTerm : conditionalParsedTerms)
		conditionalTerms.add(conditionalParsedTerm.allTerms);

	parsedTerm.isConditional = true;
	parsedTerm.conditionalTerms = conditionalTerms;
	parsedTerm.isCorrelated = isCorrelated;

	return parsedTerm;
}


// This makes 1 conditionalTerm only if terms has itself only 1 conditionalTerm, at the first or last place:
// The others parsed terms in 'terms' are added as conditional terms of this conditionalTerm.
FormulaParser::ParsedTerms FormulaParser::squeezeConditionalTerms(const ParsedTerms &terms)
{
	if (terms.length() < 2) return terms;

	bool isFirstConditional = terms[0].isConditional,
		 isLastConditional = terms[terms.length() - 1].isConditional;

	if ((!isFirstConditional && !isLastConditional) || (isFirstConditional && isLastConditional)) return terms;

	ParsedTerm conditionalTerm = isFirstConditional ? terms[0] : terms[terms.length() - 1];

	for (int i = (isFirstConditional ? 1 : 0); i < (isLastConditional ? (terms.length() - 1) : terms.length()); i++)
	{
		if (terms[i].isConditional) return terms;
		conditionalTerm.conditionalTerms.add(terms[i].allTerms);
	}

	return { conditionalTerm };
}

bool FormulaParser::parse(const QString& formula, ParsedTerms& parsedTerms, QString& error)
{
	error.clear();
	QString term;
	bool	isConditional = false,
			isCorrelated = false;
	ParsedTerms subTerms, leftCondition;

	for (int i = 0; i < formula.length(); i++)
	{
		QChar ch = formula.at(i);

		if (ch == '(')
		{
			if (!subTerms.isEmpty())
			{
				error.append(tr("Parsing error in Formula %1").arg(formula));
				return false;
			}
			int subGroup = 1;
			bool hasSubSubGroup = false;
			QString subFormula;
			int j = i+1;
			for (; j < formula.length(); j++)
			{
				QChar ch2 = formula.at(j);
				if (ch2 == '(')
				{
					subGroup ++;
					hasSubSubGroup = true;
				}
				else if (ch2 == ')')
				{
					subGroup--;
					if (subGroup == 0) break;
				}
				else subFormula.append(ch2);
			}

			if (subGroup != 0)
			{
				error.append(tr("No matching parenthesis in Formula %1").arg(formula));
				return false;
			}

			if (!parse(subFormula, subTerms, error)) return false;

			// If in the subTerms, a term is a conditional term, so it means that the whole subgroup inside the parenthesis is in fact a conditional term
			// E.g.: a + (b + c | d), means that 'b + c | d' is a conditional term, not 'c | d'
			if (!hasSubSubGroup)
				subTerms = squeezeConditionalTerms(subTerms);

			i = j+1;
		}
		else if (ch == '|')
		{
			if (term.isEmpty() && subTerms.isEmpty())
			{
				error.append(tr("Parsing error in Formula %1").arg(formula));
				return false;
			}
			isConditional = true;
			if (!term.isEmpty())
				leftCondition.append(parseTerm(term));
			else
				leftCondition = subTerms;

			if ((i + 1) < formula.length() && formula.at(i+1) == '|')
			{
				isCorrelated = false;
				i++;
			}
			else
				isCorrelated = true;

			term = QString();
			subTerms = {};
		}
		else if (ch != '+')
		{
			if (!term.isEmpty() || ch != ' ')
				term.append(ch);
		}

		if (ch == '+' || i >= formula.length() - 1)
		{
			if (!isConditional)
			{
				if (!subTerms.isEmpty() && !term.isEmpty())
				{
					error.append(tr("Syntax error in Formula %1").arg(formula));
					return false;
				}
				if (!subTerms.isEmpty())
					parsedTerms.append(subTerms);
				else if (!term.isEmpty())
					parsedTerms.append(parseTerm(term));
			}
			else
			{
				if (leftCondition.isEmpty() || (term.isEmpty() && subTerms.isEmpty()))
				{
					error.append(tr("Syntax error near conditional operator (%1) in Formula %2").arg(isCorrelated ? "|" : "||", formula));
					return false;
				}

				if (!term.isEmpty())
					// If the leftCondition has only 1 term, we cannot be sure here which one is the real term or the conditional term
					parsedTerms.append(parseTerm(term, leftCondition, isCorrelated));
				else // subTerms is not empty
				{
					if (leftCondition.length() == 1 && !leftCondition[0].isConditional)
						parsedTerms.append(parseTerm(leftCondition[0], subTerms, isCorrelated));
					else if (subTerms.length() == 1 && !subTerms[0].isConditional)
						parsedTerms.append(parseTerm(subTerms[0], leftCondition, isCorrelated));
					else
					{
						error.append(tr("Syntax error in Formula %1").arg(formula));
						return false;
					}
				}
			}

			term = QString();
			subTerms = {};
			leftCondition = {};

		}

	}
	return true;
}

QString FormulaParser::transformToFormulaTerm(const Term &term, char join, bool addQuotes)
{
	static QRegularExpression rx("^[a-zA-Z0-9_]+$");
	QString result;
	const QStringList& components = term.components();

	bool first = true;
	for (const QString& component : components)
	{
		if (!first) result += QString(' ') + join + ' ';
		first = false;

		if (addQuotes)						result += '"' + component + '"';
		else if (component.contains(rx))	result += component;
		else								result += '`' + component + '`';
	}
	return result;
}

