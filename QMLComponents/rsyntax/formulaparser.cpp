#include "formulaparser.h"
#include "variableinfo.h"
#include <QRegularExpression>
#include "log.h"
#include "workspace.h"

const char FormulaParser::interactionSeparator			= ':';
const char FormulaParser::allInterationsSeparator		= '*';

Terms FormulaParser::parseTerm(QString termStr, VariableInfoProvider * provider)
{
	auto trim = [] (const QString& input) -> QString
	{
		QString output = input.trimmed();
		if (output.startsWith('`'))		output = output.mid(1);
		if (output.endsWith('`'))		output = output.chopped(1);

		return output;
	};

	//mds-plan D8.2: split on `separator` OUTSIDE double-quoted segments. A qualified column
	//reference ("DS"."Filter"."Col"("Type"), D7.0a grammar) may contain any character inside
	//its quotes - including the interaction separators themselves - and must never be split.
	auto splitRespectingQuotes = [] (const QString& input, char separator) -> QStringList
	{
		QStringList output;
		QString current;
		bool inQuotes = false;
		for (const QChar &ch : input)
		{
			if (ch == '"')					inQuotes = !inQuotes;
			else if (ch == separator && !inQuotes)
			{
				output.append(current);
				current.clear();
				continue;
			}
			current += ch;
		}
		output.append(current);
		return output;
	};

	auto trimList = [=] (const QString& input, const char& separator) -> QStringList
	{
		QStringList output;
		if (input.contains(separator))
		{
			QStringList components = splitRespectingQuotes(input, separator);
			for (const QString& component : components)
				output.append(trim(component));
		}
		else
			output.append(trim(input));

		return output;
	};

	auto readTermWithType = [provider] (const QString& input) -> std::pair<QString, columnType>
	{
		//mds-plan D8.2: a qualified surface form is kept VERBATIM as the component - it owns
		//its ("Type") segment, and resolution into typed ids happens once, in the D8.1
		//applier (Term(json) -> resolveQualifiedTermComponent). The bare-name dot-suffix
		//logic and the provider lookup must not touch the quotes. The R-name guard wraps
		//non-syntactic names (qualified forms included) in backticks, and - unlike the whole
		//term string - split components never went through trim(), so strip them here.
		QString qualifiedCandidate = input.trimmed();
		if (qualifiedCandidate.startsWith('`'))		qualifiedCandidate = qualifiedCandidate.mid(1);
		if (qualifiedCandidate.endsWith('`'))		qualifiedCandidate = qualifiedCandidate.chopped(1);
		if (qualifiedCandidate.startsWith('"'))
			return std::make_pair(qualifiedCandidate, columnType::unknown);

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
			type = columnType(provider->provideInfo(varInfoType::VariableType, input).toInt());

		return std::make_pair(term, type);
	};

	auto readTerm = [=] (const QString& input) -> Term
	{
		QStringList components = splitRespectingQuotes(input, FormulaParser::interactionSeparator),
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

Terms FormulaParser::parseTerm(const Json::Value& jsonString, VariableInfoProvider * provider)
{
	if (jsonString.isString())
		return parseTerm(tq(jsonString.asString()), provider);
	else
	{
		Log::log() << "Wrong kind of object for the vars in formula" << jsonString.toStyledString() << std::endl;
		return Terms();
	}
}

Terms FormulaParser::parseTerms(const Json::Value& json, VariableInfoProvider * provider)
{
	Terms result;
	if (json.isNull())	
		return result;

	if (json.isString())
		result.add(parseTerm(json, provider));
	else if (json.isArray())
	{
		for (const Json::Value& col : json)
			result.add(parseTerm(col, provider));
	}
	else
	{
		Log::log() << "Wrong kind of terms in json during parseTerms: " << json.toStyledString() << std::endl;
		return result;
	}

	return result;
}

bool FormulaParser::parse(const Json::Value& formula, bool isLhs, ParsedTerms& parsedTerms, QString& error, VariableInfoProvider * provider)
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
		parsedTerms.fixedTerms = parseTerms(fixedTerms["vars"], provider);
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
				randomTerm.terms = parseTerms(randomValues["vars"], provider);
				randomTerm.correlated = randomValues["correlated"].asBool();
				randomTerm.intercept = randomValues["intercept"].asBool();
				parsedTerms.randomTerms[tq(col)] = randomTerm;
			}
		}
	}

	return true;
}

QString FormulaParser::transformToFormulaTerm(const Term &term, const Json::Value& changedType, char join, bool addQuotes, int contextFilterId)
{
	static QRegularExpression rx("^[a-zA-Z0-9_\\.]+$");
	QString result;
	const QStringList& components = term.components();
	int i = 0;

	for (QString component : components)
	{
		std::string compType = (changedType.isArray() && changedType.size() > i ? changedType[i] : changedType).asString();
		columnType type = columnTypeFromString(compType, columnType::unknown);

		//mds-plan D8.2: a term stamped with a foreign live filter id renders as the qualified
		//MINIMAL form (D7.0a renderer: bare inside the context, "DS"."Col"/"F"."Col"/full
		//outside; type segment on demand). The component string itself stays bare (C9: typed
		//terms carry names + one filterId; qualification is an emission-time act) and the
		//".type" bare suffix is not appended - the qualified form carries its own ("Type")
		//segment, and appending the bare spelling would poison the type-wins rule at re-parse.
		//An empty render (dead slice) cannot be addressed at all: the bare component is kept
		//and logged loudly - the re-parse then resolves it through the context fallback (C13),
		//which is wrong-slice data but parseable R; the loud log + the restamp on the next
		//dataSpec regeneration bound the damage. (D7.0a doctrine: never a made-up form.)
		if (contextFilterId >= 0 && term.filterId() >= 0 && term.filterId() != contextFilterId)
		{
			QString qualified;
			if (Workspace * ws = Workspace::singleton())
				qualified = ws->renderQualifiedColumnRef(contextFilterId, term.filterId(), component.toStdString(), type);

			if (!qualified.isEmpty())
				component = qualified;
			else
				Log::log() << "FormulaParser::transformToFormulaTerm: term component '" << component.toStdString()
						   << "' is stamped with filter id " << term.filterId()
						   << " which renders to nothing (dead slice); emitting the bare name under the context namespace" << std::endl;
		}
		else if (type != columnType::unknown)
			component.append("." + tq(compType));

		if (i > 0)
			result += QString(' ') + join + ' ';

		if (addQuotes)
		{
			//A qualified form contains double quotes: escape them (and any backslash) for the R
			//string literal. Bare components keep the legacy verbatim wrap (byte-identity).
			QString literal = component;
			if (literal.contains('"'))
				literal.replace("\\", "\\\\").replace("\"", "\\\"");
			result += '"' + literal + '"';
		}
		else if (component.contains(rx))	result += component;
		else								result += '`' + component + '`';
		i++;
	}
	return result;
}

