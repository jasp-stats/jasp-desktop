#include "formulaparser.h"
#include <QRegularExpression>
#include "log.h"

const char FormulaParser::interactionSeparator			= ':';
const char FormulaParser::allInterationsSeparator		= '*';

Terms FormulaParser::parseTerm(QString term)
{
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

	Terms result;
	if (term.contains(FormulaParser::interactionSeparator))
	{
		term.replace(FormulaParser::interactionSeparator, Term::separator);
		result.add(Term::readTerm(term));
	}
	else if (term.contains(FormulaParser::allInterationsSeparator))
	{
		Terms baseTerms = trimList(term, FormulaParser::allInterationsSeparator);
		result = baseTerms.crossCombinations();
	}
	else
		result.add(term);

	return result;
}

Terms FormulaParser::parseTerm(const Json::Value& jsonString)
{
	if (jsonString.isString())
		return parseTerm(tq(jsonString.asString()));
	else
	{
		Log::log() << "Wrong kind of object for the vars in formula" << jsonString.toStyledString() << std::endl;
		return Terms();
	}
}

Terms FormulaParser::parseTerms(const Json::Value& json)
{
	Terms result;
	if (json.isNull())	return result;

	if (json.isString())
		result.add(parseTerm(json));
	else if (json.isArray())
	{
		for (const Json::Value& col : json)
			result.add(parseTerm(col));
	}
	else
	{
		Log::log() << "Wrong kind of terms in json during parseTerms: " << json.toStyledString() << std::endl;
		return result;
	}

	return result;
}

bool FormulaParser::parse(const Json::Value& formula, bool isLhs, ParsedTerms& parsedTerms, QString& error)
{
	error.clear();

	if (formula.isNull())	return true;

	if (!formula.isObject())
	{
		error.append("Wrong type of formula object");
		return false;
	}

	const Json::Value& fixedTerms = isLhs ? formula : formula["fixed"];

	if (!fixedTerms.isNull())
	{
		if (!fixedTerms.isObject())
		{
			error.append("Wrong type of fixed terms");
			return false;
		}

		parsedTerms.intercept = fixedTerms["intercept"].asBool();
		parsedTerms.fixedTerms = parseTerms(fixedTerms["vars"]);
	}

	if (!isLhs)
	{
		const Json::Value& randomTerms = formula["random"];
		if (!randomTerms.isNull())
		{
			if (!randomTerms.isObject())
			{
				error.append("Wrong type of random terms object");
				return false;
			}
			for (const std::string& col : randomTerms.getMemberNames())
			{
				const Json::Value& randomValues = randomTerms[col];
				RandomTerm randomTerm;
				randomTerm.terms = parseTerms(randomValues["vars"]);
				randomTerm.correlated = randomValues["correlated"].asBool();
				randomTerm.intercept = randomValues["intercept"].asBool();
				parsedTerms.randomTerms[tq(col)] = randomTerm;
			}
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
		if (!first) 
			result += QString(' ') + join + ' ';

		first = false;

		if (addQuotes)						result += '"' + component + '"';
		else if (component.contains(rx))	result += component;
		else								result += '`' + component + '`';
	}
	return result;
}

