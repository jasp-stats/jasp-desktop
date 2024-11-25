#include "formulaparser.h"
#include "variableinfo.h"
#include <QRegularExpression>
#include "log.h"

const char FormulaParser::interactionSeparator			= ':';
const char FormulaParser::allInterationsSeparator		= '*';

Terms FormulaParser::parseTerm(QString termStr)
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

	auto readTermWithType = [] (const QString& input) -> std::pair<QString, columnType>
	{
		QString term = input;
		columnType type = columnType::unknown;
		int index = input.lastIndexOf('.');
		if (index > 0)
		{
			QString typeStr = input.mid(index+1);
			if (columnTypeValidName(fq(typeStr)))
			{
				type = columnTypeFromQString(typeStr);
				term = input.first(index);
			}
		}
		if (type == columnType::unknown)
			type = columnType(VariableInfo::info()->provider()->provideInfo(VariableInfo::VariableType, input).toInt());

		return std::make_pair(term, type);
	};

	auto readTerm = [=] (const QString& input) -> Term
	{
		QStringList components = input.split(FormulaParser::interactionSeparator),
					parsedComponents;
		columnTypeVec types;
		for (const QString& component : components)
		{
			std::pair<QString, columnType> pair = readTermWithType(component);
			parsedComponents.push_back(pair.first);
			types.push_back(pair.second);
		}

		return Term(parsedComponents, types);
	};

	Terms result;

	if (termStr.contains(FormulaParser::allInterationsSeparator))
	{
		Terms baseTerms;
		QStringList baseTermsStr = trimList(termStr, FormulaParser::allInterationsSeparator);
		for (const QString& baseTermStr : baseTermsStr)
			baseTerms.add(readTerm(baseTermStr));
		result = baseTerms.crossCombinations();
	}
	else
		result.add(readTerm(trim(termStr)));

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

QString FormulaParser::transformToFormulaTerm(const Term &term, const Json::Value& changedType, char join, bool addQuotes)
{
	static QRegularExpression rx("^[a-zA-Z0-9_\\.]+$");
	QString result;
	const QStringList& components = term.components();
	int i = 0;

	for (QString component : components)
	{
		std::string compType = (changedType.isArray() && changedType.size() > i ? changedType[i] : changedType).asString();
		if (columnTypeFromString(compType, columnType::unknown) != columnType::unknown)
			component.append("." + tq(compType));

		if (i > 0)
			result += QString(' ') + join + ' ';

		if (addQuotes)						result += '"' + component + '"';
		else if (component.contains(rx))	result += component;
		else								result += '`' + component + '`';
		i++;
	}
	return result;
}

