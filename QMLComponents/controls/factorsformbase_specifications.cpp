//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

// Term specification of a FactorsForm: opt-in with `advancedAvailable: true` in QML (it must not change at runtime); the
// property `advanced` switches it on (the "Specify" button, or `advanced: true` for new analyses). Without
// `advancedAvailable` the option value, its `.meta`, `.filterIds` and Storage are exactly the basic ones.
//
// The specifications (their kind, parameters, defaults and the label of their term) are listed once, in jaspBase's
// inst/modelTermSpecifications.json, embedded at build time (catalogue() below); jaspBase::prepareModelTerms() reads the
// same file and labels the terms the same way. A new specification needs no code here.
//
// How it works:
//   1. Definitions (`_definitions`, edited in the "Specify" table, FactorsFormDefinitions.qml) are the terms all models
//      can use, defined once. A definition is a variable with a specification: a transformation of a continuous
//      variable (`none`, `log`, `polynomial`, ..., or `offset`: the variable with its coefficient fixed at 1) or a coding
//      of a categorical one (`treatment`, `sum`, ...), with its parameters and an optional restriction (fixed
//      coefficients). A constant offset (`offsetConstant`) has a number instead of a variable. Which specifications a
//      form offers: `continuousSpecifications` / `categoricalSpecifications`, where "default" stands for every one the
//      catalogue offers by default. The analysis' variables come from the hidden list `variablesUniverse`
//      (FactorsForm.qml), which has the available list's own source. Every variable has a base definition (its first
//      one, created as the plain variable, never removed); further definitions are added as empty rows. Unfinished rows
//      (no variable, or no number) are no terms and are not saved: a rebind of the option (loading, RPC) drops them.
//   2. Labels: a complete definition gives a term label ("x", "log(x)", "f (reference b)", "offset(2)"). While the
//      specification is on, the available list offers "(Intercept)" and these labels (`definitionTerms`) instead of the
//      variables, so the models hold labels and build interactions from them as usual ("log(x) * z"). The intercept and
//      offsets are never part of an interaction.
//   3. A label that changes (an edit, a column or level renamed in the data, reordered levels) is renamed in the
//      available list, which renames it in every model, also inside interactions; `_labels` remembers the labels the
//      models hold. A removed definition takes its terms out of every model. No two definitions give the same label
//      (_resolveLabelClashes).
//   4. Option value, per model while the specification is on. Engine (what R gets; jaspBase::prepareModelTerms() reads
//      it): the optionKey member (`components` with `allowInteraction`: the variables of its terms, as in basic mode),
//      `intercept`, `interceptRestriction` (with `restrictionAvailable`), `definitions` (the ones the model uses, in the
//      term-option shape {value, types, optionKey: "variable"}, so the column encoder encodes their variables) and
//      `terms` ([{id, operands: [definition ids]}]). Storage (the .jasp file): the model's terms as labels, `advanced`,
//      every complete definition with every parameter ever set and the label it had (a later version that labels it
//      differently renames the term on loading) and `interceptRestriction`. An Engine value that cannot be bound as it is
//      (RPC) is refused (_advancedJsonValid).
//   5. Switching on gives every model "(Intercept)" and the base term of each of its variables; switching off (Reset)
//      gives every model the variables of its terms (nested again when the form is nested).

#include "factorsformbase.h"
#include "variableslistbase.h"
#include "models/listmodeldraggable.h"
#include "variableinfo.h"
#include "qutils.h"
#include "log.h"
#include "analysisform.h"
#include <QLocale>
#include <QRegularExpression>
#include <cmath>
#include <map>
#include <set>

namespace
{
	const QString interceptLabel	= "(Intercept)",
				  offsetChoice		= "(Constant offset)";	///< the "variable" an added row chooses to become a constant offset

	bool isOffset(const QString & specification)	{ return specification == "offset" || specification == "offsetConstant"; }	///< coefficient fixed at 1

	///< A constant offset's value: a finite number.
	bool isFiniteNumber(const QString & text, double * number = nullptr)
	{
		bool ok = false;
		const double value = text.trimmed().toDouble(&ok);
		if (number)
			*number = value;
		return ok && std::isfinite(value);
	}

	// The catalogue of specifications (jaspBase inst/modelTermSpecifications.json, which documents its fields) ---------

	struct Parameter
	{
		QString			name,
						type,			///< number, integer, location (mean, median or a number), choice or level
						title,
						defaultValue,	///< a level's default is the first level
						placeholder;
		int				min = 0;
		QVariantList	choices;		///< {value, title, short}
	};

	struct Specification
	{
		QString					name,
								kind,			///< continuous, categorical or constant
								title,
								label;			///< the term: {x}, {parameter}, {parameter|filter}, [optional]
		bool					byDefault = false;
		std::vector<Parameter>	parameters;
	};

	const std::vector<Specification> & catalogue()
	{
		static const std::vector<Specification> specifications = []()
		{
			const std::string text =
				#include "jaspBase_modelTermSpecifications.h"
				;

			Json::Value json;
			if (!Json::Reader().parse(text, json))
				Log::log() << "FactorsForm: the catalogue of term specifications cannot be read" << std::endl;

			std::vector<Specification> result;
			for (const Json::Value & entry : json.get("specifications", Json::arrayValue))
			{
				Specification specification;
				specification.name		= tq(entry.get("name", "").asString());
				specification.kind		= tq(entry.get("kind", "").asString());
				specification.title		= tq(entry.get("title", "").asString());
				specification.label		= tq(entry.get("label", "").asString());
				specification.byDefault	= entry.get("default", false).asBool();
				for (const Json::Value & parameterJson : entry.get("parameters", Json::arrayValue))
				{
					Parameter parameter;
					parameter.name			= tq(parameterJson.get("name", "").asString());
					parameter.type			= tq(parameterJson.get("type", "number").asString());
					parameter.title			= tq(parameterJson.get("title", "").asString());
					parameter.defaultValue	= tq(parameterJson.get("default", "").asString());
					parameter.placeholder	= tq(parameterJson.get("placeholder", "").asString());
					parameter.min			= parameterJson.get("min", 0).asInt();
					for (const Json::Value & choice : parameterJson.get("choices", Json::arrayValue))
						parameter.choices.append(QVariantMap{ { "value", tq(choice.get("value", "").asString()) }, { "title", tq(choice.get("title", "").asString()) },
															  { "short", tq(choice.get("short", "").asString()) } });
					specification.parameters.push_back(parameter);
				}
				result.push_back(specification);
			}
			return result;
		}();

		return specifications;
	}

	const Specification * specificationNamed(const QString & name)
	{
		for (const Specification & specification : catalogue())
			if (specification.name == name)
				return &specification;
		return nullptr;
	}

	const Parameter * parameterOf(const QString & specification, const QString & name)
	{
		if (const Specification * found = specificationNamed(specification))
			for (const Parameter & parameter : found->parameters)
				if (parameter.name == name)
					return &parameter;
		return nullptr;
	}

	///< The parameters a specification uses.
	QStringList parametersOf(const QString & specification)
	{
		QStringList names;
		if (const Specification * found = specificationNamed(specification))
			for (const Parameter & parameter : found->parameters)
				names.append(parameter.name);
		return names;
	}

	bool isParameter(const QString & name)
	{
		for (const Specification & specification : catalogue())
			for (const Parameter & parameter : specification.parameters)
				if (parameter.name == name)
					return true;
		return false;
	}

	///< A parameter as labels and signatures use it: trimmed, except a level, whose spaces are its own.
	QString settingOf(const QString & specification, const QMap<QString, QString> & parameters, const QString & name)
	{
		const Parameter * parameter = parameterOf(specification, name);
		return parameter && parameter->type == "level" ? parameters.value(name) : parameters.value(name).trimmed();
	}

	///< A plain number, written without parentheses in a label (as jaspBase writes it).
	bool plainNumber(const QString & text) { static const QRegularExpression plain("^[0-9]+(\\.[0-9]+)?$"); return plain.match(text).hasMatch(); }

	bool isCategorical(columnType type)	{ return type == columnType::nominal || type == columnType::nominalText || type == columnType::ordinal; }	///< as jaspBase reads factors

	bool boolOr(const Json::Value & value, bool fallback)					{ return value.isBool() ? value.asBool() : fallback; }
	QString stringOr(const Json::Value & value, const QString & fallback)	{ return value.isString() ? tq(value.asString()) : fallback; }

	QString scalarAsString(const Json::Value & value)
	{
		if (value.isString())	return tq(value.asString());
		if (value.isInt())		return QString::number(value.asInt());
		if (value.isDouble())	return QString::number(value.asDouble(), 'g', QLocale::FloatingPointShortest);
		if (value.isBool())		return value.asBool() ? "true" : "false";

		return QString();
	}

	///< Rows arrive as the Engine term-option object {value, types, optionKey} or as a plain array (Storage, RPC).
	Json::Value rowsOf(const Json::Value & member)
	{
		if (member.isObject() && member.isMember("value"))
			return member["value"];

		return member.isArray() ? member : Json::Value(Json::arrayValue);
	}

	int idNumber(const QString & id)
	{
		bool ok = false;
		int number = id.mid(1).toInt(&ok);
		return ok ? number : 0;
	}

	QStringList stringList(const Json::Value & value)
	{
		QStringList result;
		if (value.isString())
			result.append(tq(value.asString()));
		else if (value.isArray())
			for (const Json::Value & element : value)
				if (element.isString())
					result.append(tq(element.asString()));
		return result;
	}
}

// Variables, types and labels -----------------------------------------------------------------------------------------

std::vector<std::pair<QString, columnType>> FactorsFormBase::_universe() const
{
	std::vector<std::pair<QString, columnType>> variables;
	const ListModel * model = _universeList ? _universeList->model() : nullptr;

	if (model)
		for (const Term & term : model->terms())
			if (term.components().size() == 1)
				variables.push_back({ term.components()[0], term.type() });

	return variables;
}

///< The variables are known once the hidden list has been set up (while a file is loaded they are not yet).
bool FactorsFormBase::_universeKnown() const
{
	return _universeList && _universeList->initialized();
}

columnType FactorsFormBase::_typeOf(const QString & variable) const
{
	for (const auto & [name, type] : _universe())
		if (name == variable)
			return type;

	return columnType::unknown;
}

///< The dataset/filter a variable comes from, as its term in the hidden list says (-1: not stamped).
int FactorsFormBase::_filterIdOf(const QString & variable) const
{
	const ListModel * model = _universeList ? _universeList->model() : nullptr;

	if (model)
		for (const Term & term : model->terms())
			if (term.components().size() == 1 && term.components()[0] == variable)
				return term.filterId();

	return -1;
}

///< The specifications the form offers for a variable of this type, in the order of the form's list; "default" stands for
/// every one of the kind that the catalogue offers by default (so forms that keep it get new ones of later versions).
QStringList FactorsFormBase::_allowedSpecifications(columnType type) const
{
	const QString		kind	= isCategorical(type) ? "categorical" : "continuous";
	const QStringList &	wanted	= isCategorical(type) ? _categoricalSpecifications : _continuousSpecifications;
	QStringList			allowed;

	for (const QString & name : wanted)
	{
		QStringList names(name);
		if (name == "default")
		{
			names.clear();
			for (const Specification & specification : catalogue())
				if (specification.kind == kind && specification.byDefault)
					names.append(specification.name);
		}

		for (const QString & candidate : names)
		{
			const Specification * specification = specificationNamed(candidate);
			if (specification && specification->kind == kind)
			{
				if (!allowed.contains(candidate))
					allowed.append(candidate);
			}
			// (an unknown name is ignored; _connectUniverse logs it)
		}
	}

	return allowed;
}

///< An added row may become a constant offset when the form lists "offsetConstant" (in continuousSpecifications).
bool FactorsFormBase::_constantOffsetAllowed() const
{
	return _continuousSpecifications.contains("offsetConstant");
}

QString FactorsFormBase::_defaultSpecification(columnType type) const
{
	const QStringList	allowed		= _allowedSpecifications(type);
	const QString		preferred	= isCategorical(type) ? "treatment" : "none";

	return allowed.contains(preferred) ? preferred : allowed.value(0, preferred);
}

QStringList FactorsFormBase::_levels(const QString & variable) const
{
	const ListModel * model = _universeList ? _universeList->model() : _factorsModel;
	return model ? model->requestInfo(varInfoType::Labels, variable).toStringList() : QStringList();
}

void FactorsFormBase::_setDefaultParameters(TermDefinition & definition) const
{
	if (const Specification * specification = specificationNamed(definition.specification))
		for (const Parameter & parameter : specification->parameters)
			if (definition.parameters.value(parameter.name).isEmpty())
				definition.parameters[parameter.name] = parameter.type == "level" ? _levels(definition.variable).value(0) : parameter.defaultValue;
}

///< What makes two definitions the same: the variable, the specification and the values of its own parameters (a
/// constant offset: its value as a number).
QString FactorsFormBase::_signature(const TermDefinition & definition) const
{
	double value = 0;
	if (definition.specification == "offsetConstant" && isFiniteNumber(definition.parameters.value("value"), &value))
		return "offsetConstant|" + QString::number(value, 'g', 17);

	QString signature = definition.variable + "|" + definition.specification;
	for (const QString & parameter : parametersOf(definition.specification))
		signature += "|" + settingOf(definition.specification, definition.parameters, parameter);

	return signature;
}

///< The term a definition gives: the label pattern of its specification in the catalogue, filled in (jaspBase's
/// .modelTermsLabel() fills it in the same way). It is not translated: it is the value the models store, so it is
/// assumed not to contain the term separator " * " (column and level names could). In the pattern, {x} is the variable,
/// {parameter} its value, {parameter|paren} parenthesized unless a plain number, {parameter|signed} " + v" or " - v",
/// {parameter|choice} the short text of the choice, {parameter|number} the number written shortest; [ ... ] is left out
/// while every parameter in it has its default (a reference level: the first level). A column named "(Intercept)" is
/// written `(Intercept)`, so it is not taken for the intercept.
QString FactorsFormBase::_label(const TermDefinition & definition) const
{
	const Specification * specification = specificationNamed(definition.specification);
	if (!specification)
		return definition.variable + " (" + definition.specification + ")";

	auto value = [&](const QString & name) { return settingOf(definition.specification, definition.parameters, name); };
	auto atDefault = [&](const QString & name)
	{
		const Parameter * parameter = parameterOf(definition.specification, name);
		if (!parameter)							return true;
		if (parameter->type == "level")			return value(name).isEmpty() || value(name) == _levels(definition.variable).value(0);
		return value(name) == parameter->defaultValue;
	};
	auto filled = [&](const QString & placeholder) -> QString
	{
		const QString name = placeholder.section('|', 0, 0), filter = placeholder.section('|', 1);
		if (name == "x")				return definition.variable;
		const QString text = value(name);
		if (filter == "paren")			return plainNumber(text) ? text : "(" + text + ")";
		if (filter == "signed")			return text.startsWith('-') ? " - " + text.mid(1) : " + " + text;
		if (filter == "number")			return QString::number(text.toDouble(), 'g', QLocale::FloatingPointShortest);
		if (filter == "choice")
			if (const Parameter * parameter = parameterOf(definition.specification, name))
				for (const QVariant & choice : parameter->choices)
					if (choice.toMap().value("value").toString() == text)
						return choice.toMap().value("short").toString();
		return text;
	};

	// One pass over the pattern: text is copied, {placeholders} are filled in, an [optional part] is kept only when one
	// of its parameters differs from its default.
	QString label, optional;
	bool inOptional = false, keep = false;
	const QString & pattern = specification->label;
	for (int i = 0; i < pattern.size(); i++)
	{
		QString piece = pattern[i];
		if (pattern[i] == '{')
		{
			const int close = pattern.indexOf('}', i);
			if (close < 0)
				break;	// a malformed pattern (jaspBase's tests check the catalogue)
			const QString placeholder = pattern.mid(i + 1, close - i - 1);
			piece = filled(placeholder);
			if (inOptional && placeholder != "x" && !atDefault(placeholder.section('|', 0, 0)))
				keep = true;
			i = close;
		}
		else if (pattern[i] == '[')	{ inOptional = true; keep = false; optional.clear(); continue; }
		else if (pattern[i] == ']')	{ inOptional = false; if (keep) label += optional; continue; }

		(inOptional ? optional : label) += piece;
	}

	return label == interceptLabel ? "`" + label + "`" : label;
}

///< A row that gives a term: a variable, or a constant offset with its value (an added row starts without either).
bool FactorsFormBase::_isComplete(const TermDefinition & definition) const
{
	return definition.specification == "offsetConstant" ? isFiniteNumber(definition.parameters.value("value")) : !definition.variable.isEmpty();
}

const FactorsFormBase::TermDefinition * FactorsFormBase::_definitionWithLabel(const QString & label) const
{
	for (const TermDefinition & definition : _definitions)
		if (_isComplete(definition) && _label(definition) == label)
			return &definition;

	return nullptr;
}

///< The base definition of a variable is its first one: always there, its variable fixed, not removable.
bool FactorsFormBase::_isBase(const TermDefinition & definition) const
{
	if (definition.variable.isEmpty())
		return false;

	for (const TermDefinition & other : _definitions)
		if (other.variable == definition.variable)
			return other.id == definition.id;

	return false;
}

///< The label of every complete definition, by id: the terms the models can hold.
QMap<QString, QString> FactorsFormBase::_completeLabels() const
{
	QMap<QString, QString> labels;
	for (const TermDefinition & definition : _definitions)
		if (_isComplete(definition))
			labels[definition.id] = _label(definition);

	return labels;
}

QStringList FactorsFormBase::definitionTerms() const
{
	QStringList terms;
	if (!_advancedAvailable)
		return terms;

	if (_interceptMode == "optional")
		terms.append(interceptLabel);

	// (until the variables are known, e.g. while a file is loaded, every definition: _syncDefinitions keeps them too)
	const auto universe = _universe();
	for (const TermDefinition & definition : _definitions)
		if (_isComplete(definition) && (definition.specification == "offsetConstant" || !_universeKnown() || std::any_of(universe.begin(), universe.end(), [&](const auto & variable) { return variable.first == definition.variable; })))
			terms.append(_label(definition));

	return terms;
}

///< The terms a new model starts with while the specification is on: its intercept term.
Terms FactorsFormBase::initialModelTerms() const
{
	Terms terms;
	if (advancedActive() && _interceptMode == "optional")
		terms.add(Term(interceptLabel));

	return terms;
}

// Definitions -----------------------------------------------------------------------------------------------------------

///< The base definition of a variable: the plain variable (`none`, or `treatment` with its first level).
FactorsFormBase::TermDefinition FactorsFormBase::_newDefinition(const QString & variable)
{
	TermDefinition definition;
	definition.id				= "d" + QString::number(_nextDefinitionId++);
	definition.variable			= variable;
	definition.specification	= _defaultSpecification(_typeOf(variable));
	_setDefaultParameters(definition);
	return definition;
}

void FactorsFormBase::_syncDefinitions()
{
	if (!_advancedAvailable || !_universeKnown())
		return;	// the variables are not known yet (a file is being loaded): keep the definitions that were read

	// Once the whole form is bound (a loaded analysis has its variables by then), a variable without definition that
	// was not there before is one the user just added.
	const bool	userAddition	= form() && form()->initialized();
	const auto	universe		= _universe();

	// Definitions follow the variables of the analysis: first the base definition of every variable (in their order;
	// a new variable gets its plain one), then the added definitions in the order they were added (unfinished rows and
	// constant offsets too). The definitions of a removed variable go, and their terms leave every model with the
	// available list.
	std::vector<TermDefinition>	ordered;
	QStringList					variables, newVariables;
	for (const auto & [variable, type] : universe)
	{
		variables.append(variable);
		auto base = std::find_if(_definitions.begin(), _definitions.end(), [&](const TermDefinition & definition) { return definition.variable == variable; });
		if (base != _definitions.end())
			ordered.push_back(*base);
		else
		{
			ordered.push_back(_newDefinition(variable));
			if (userAddition && !_knownVariables.contains(variable))
				newVariables.append(variable);
		}
	}
	for (const TermDefinition & definition : _definitions)
		if (definition.variable.isEmpty() || (variables.contains(definition.variable) && !std::any_of(ordered.begin(), ordered.end(), [&](const TermDefinition & other) { return other.id == definition.id; })))
			ordered.push_back(definition);

	// A specification the variable's (changed) type or the analysis does not allow falls back to the default; of two
	// definitions that became the same, the first stays (labels that clash: _resolveLabelClashes).
	QSet<QString> signatures;
	_definitions.clear();
	for (TermDefinition definition : ordered)
	{
		if (!definition.variable.isEmpty() && !_allowedSpecifications(_typeOf(definition.variable)).contains(definition.specification))
		{
			definition.specification = _defaultSpecification(_typeOf(definition.variable));
			_setDefaultParameters(definition);
		}
		if (_isComplete(definition))
		{
			if (signatures.contains(_signature(definition)))
				continue;
			signatures.insert(_signature(definition));
		}
		_definitions.push_back(definition);
	}

	_definitionsEdited();

	// A variable added to the analysis while the specification is on goes, as its base term, into the model that takes
	// new variables in basic mode too (assignAvailableVariablesToList).
	const int lastModel = property("assignAvailableVariablesToList").toInt();
	if (advancedActive() && initialized() && lastModel >= 0 && lastModel < _factorsModel->count())
		for (const QString & variable : newVariables)
			for (const TermDefinition & definition : _definitions)
				if (definition.variable == variable)
				{
					Factor				*	factor	= _factorsModel->factor(lastModel);
					ListModelDraggable	*	model	= factor->listView ? qobject_cast<ListModelDraggable *>(factor->listView->model()) : nullptr;
					if (model && !model->terms().containsValue(_label(definition)))
					{
						Terms term;
						term.add(Term(QStringList(_label(definition)), columnTypeVec({ _typeOf(variable) })));
						model->addTerms(term);
					}
					break;
				}

	_knownVariables = variables;
}

///< The models identify terms by their label, so no two definitions may give the same label, nor "(Intercept)". Edits
/// that would are refused (setDefinitionValue), but a column renamed or added under the name of a defined term (a
/// column "log(x)" while log(x) is defined) still makes a clash: the variable as it is keeps its name, another base
/// definition falls back to the plain variable, and an added definition goes. Returns the labels the models held for
/// those that gave way: these terms would now mean another definition.
QStringList FactorsFormBase::_resolveLabelClashes()
{
	QSet<QString>	labels,
					plain;		///< ids of the variables as they are
	QStringList		gone;

	for (const TermDefinition & definition : _definitions)
		if (_isComplete(definition) && _label(definition) == definition.variable && !labels.contains(definition.variable))
		{
			labels.insert(definition.variable);
			plain.insert(definition.id);
		}

	for (auto definition = _definitions.begin(); definition != _definitions.end(); )
	{
		if (_isComplete(*definition) && !plain.contains(definition->id) && labels.contains(_label(*definition)))
		{
			if (_labels.contains(definition->id))
				gone.append(_labels.take(definition->id));

			if (_isBase(*definition))
			{
				definition->specification = _defaultSpecification(_typeOf(definition->variable));
				definition->parameters.remove("referenceLevel");
				_setDefaultParameters(*definition);
			}
			if (!_isBase(*definition) || labels.contains(_label(*definition)))
			{
				definition = _definitions.erase(definition);
				continue;
			}
		}

		if (_isComplete(*definition))
			labels.insert(_label(*definition));
		definition++;
	}

	return gone;
}

///< Takes every term made with one of `labels` out of every model (with the specification off they hold variables).
void FactorsFormBase::_removeTermsWith(const QStringList & labels)
{
	auto uses = [&](const Term & term)
	{
		for (const QString & component : term.components())
			if (labels.contains(component))
				return true;
		return false;
	};

	if (!advancedActive())
		return;

	for (int i = 0; _factorsModel && i < _factorsModel->count(); i++)
	{
		Factor				*	factor	= _factorsModel->factor(i);
		ListModelDraggable	*	model	= factor->listView ? qobject_cast<ListModelDraggable *>(factor->listView->model()) : nullptr;
		const Terms			&	terms	= model ? model->terms() : factor->initTerms;
		QList<int>				used;
		Terms					kept;

		for (size_t j = 0; j < terms.size(); j++)
			if (uses(terms.at(j)))	used.append(int(j));
			else					kept.add(terms.at(j));

		if (!model)					factor->initTerms = kept;
		else if (!used.isEmpty())	model->removeTerms(used);
	}
}

void FactorsFormBase::_definitionsEdited()
{
	// A term whose label would now mean another definition leaves the models first.
	const QStringList gone = _resolveLabelClashes();
	if (!gone.isEmpty())
	{
		_removeTermsWith(gone);
		addControlWarningTemporary(tr("%1 left the models: a variable or another term has this name now.").arg(gone.join(", ")));
	}

	// The models hold labels: a label that changed is renamed in the available list, which renames it in every model
	// (also inside interactions) before the available list gets the new set of terms, so the term stays assigned. (While
	// the specification is off the models hold variables.)
	const QMap<QString, QString> labels = _completeLabels();
	QMap<QString, QString> renamed;
	for (auto it = labels.begin(); it != labels.end(); ++it)
		if (_labels.contains(it.key()) && _labels.value(it.key()) != it.value())
			renamed[_labels.value(it.key())] = it.value();
	_labels = labels;

	if (!renamed.isEmpty() && advancedActive() && _availableVariablesListItem && _availableVariablesListItem->model())
		_availableVariablesListItem->model()->sourceVariableNamesChanged(renamed);

	_specificationsRevision++;
	emit specificationsChanged();
	emit definitionTermsChanged();

	if (initialized())
		setBoundValue(createEngineValue());
}

///< A column renamed in the data: its definitions follow, and so do their terms in the models (_definitionsEdited).
void FactorsFormBase::_variablesRenamed(const QMap<QString, QString> & renamed)
{
	if (!_advancedAvailable)
		return;

	bool changed = false;
	for (TermDefinition & definition : _definitions)
		if (renamed.contains(definition.variable))
		{
			definition.variable = renamed.value(definition.variable);
			changed = true;
		}

	if (changed)
		_definitionsEdited();
}

///< Levels of a nominal column renamed in the data: a treatment coding keeps its reference level under its new name.
void FactorsFormBase::_levelsRenamed(const QString & variable, const QMap<QString, QString> & renamed)
{
	if (!_advancedAvailable)
		return;

	for (TermDefinition & definition : _definitions)
		if (definition.variable == variable && renamed.contains(definition.parameters.value("referenceLevel")))
			definition.parameters["referenceLevel"] = renamed.value(definition.parameters.value("referenceLevel"));

	_definitionsEdited();	// a label shows the reference level, or not when it is the first level
}

QVariantList FactorsFormBase::definitionRows() const
{
	QVariantList rows;

	QStringList variables;
	for (const auto & [variable, type] : _universe())
		variables.append(variable);

	if (_interceptMode != "none")
	{
		QVariantMap row;
		row["kind"]			= "intercept";
		row["id"]			= "intercept";
		row["label"]		= interceptLabel;
		row["restriction"]	= _interceptRestriction;
		row["removable"]	= false;
		rows.append(row);
	}

	for (const TermDefinition & definition : _definitions)
	{
		if (!definition.variable.isEmpty() && _universeKnown() && !variables.contains(definition.variable))
			continue;

		const columnType type = _typeOf(definition.variable);

		QVariantMap parameters;
		for (const QString & parameter : definition.parameters.keys())
			parameters[parameter] = definition.parameters.value(parameter);

		// what the table offers: the allowed specifications with their titles, the editors of this one's parameters
		QVariantList choices, parameterInfo;
		if (!definition.variable.isEmpty())
			for (const QString & name : _allowedSpecifications(type))
				choices.append(QVariantMap{ { "value", name }, { "title", specificationNamed(name)->title } });
		if (const Specification * specification = specificationNamed(definition.specification))
			for (const Parameter & parameter : specification->parameters)
				parameterInfo.append(QVariantMap{ { "name", parameter.name }, { "type", parameter.type }, { "title", parameter.title },
												  { "min", parameter.min }, { "placeholder", parameter.placeholder }, { "choices", parameter.choices } });

		QVariantMap row;
		row["kind"]				= "definition";
		row["id"]				= definition.id;
		row["variable"]			= definition.variable;
		row["variables"]		= variables;
		row["specification"]	= definition.specification;
		row["parameters"]		= parameters;
		row["restriction"]		= definition.restriction;
		row["choices"]			= choices;
		row["parameterInfo"]	= parameterInfo;
		row["offsetChoice"]		= _constantOffsetAllowed() ? offsetChoice : QString();
		row["levels"]			= isCategorical(type) ? _levels(definition.variable) : QStringList();
		row["base"]				= _isBase(definition);
		row["removable"]		= !_isBase(definition);
		rows.append(row);
	}

	return rows;
}

void FactorsFormBase::setDefinitionValue(QString id, QString key, QString value)
{
	if (id == "intercept")
	{
		if (key == "restriction" && _interceptRestriction != value)
		{
			_interceptRestriction = value;
			_definitionsEdited();
		}
		return;
	}

	auto definition = std::find_if(_definitions.begin(), _definitions.end(), [&](const TermDefinition & candidate) { return candidate.id == id; });
	if (definition == _definitions.end())
		return;

	// Two definitions may not be the same, nor give the same label (the models identify terms by their label).
	TermDefinition edited = *definition;
	auto conflicts = [&](const TermDefinition & candidate)
	{
		if (!_isComplete(candidate))
			return false;
		const QString label = _label(candidate);
		return std::any_of(_definitions.begin(), _definitions.end(), [&](const TermDefinition & other)
		{
			return other.id != candidate.id && _isComplete(other) && (_signature(other) == _signature(candidate) || _label(other) == label);
		});
	};

	const bool constant = definition->specification == "offsetConstant";
	if ((key == "variable" && _isBase(*definition)) || (key == "value" && !constant) || (key != "variable" && key != "value" && definition->variable.isEmpty()))
		return;	// a base definition keeps its variable; an empty row gets its variable (or becomes a constant offset) first

	if (key == "variable" && (value == offsetChoice ? constant : value == definition->variable))
		return;	// the same choice again (a dropdown also reports it)

	if (key == "variable" && value == offsetChoice)
	{
		if (!_constantOffsetAllowed())
			return;
		edited.variable.clear();
		edited.specification	= "offsetConstant";
		edited.parameters.clear();
		edited.restriction.clear();
	}
	else if (key == "value")
	{
		if (!value.trimmed().isEmpty() && !isFiniteNumber(value))
		{
			addControlWarningTemporary(tr("A constant offset needs a number, not '%1'.").arg(value));
			_specificationsRevision++;	// the view shows the stored value again
			emit specificationsChanged();
			return;
		}
		edited.parameters["value"] = value.trimmed();
	}
	else if (key == "variable" && value.isEmpty())
	{
		edited.variable.clear();
		edited.specification.clear();
		edited.parameters.clear();
	}
	else if (key == "variable")
	{
		// another variable: its type's default specification (or the first free one) and its own levels
		if (constant)
		{
			edited.specification.clear();
			edited.parameters.clear();
		}
		edited.variable = value;
		edited.parameters.remove("referenceLevel");
		const QStringList allowed = _allowedSpecifications(_typeOf(value));
		if (!allowed.contains(edited.specification))
			edited.specification = _defaultSpecification(_typeOf(value));
		_setDefaultParameters(edited);
		for (int i = 0; conflicts(edited) && i < allowed.size(); i++)
		{
			edited.specification = allowed[i];
			_setDefaultParameters(edited);
		}
	}
	else if (key == "specification")		{ edited.specification = value; _setDefaultParameters(edited); }
	else if (key == "restriction")			edited.restriction = value;
	else if (isParameter(key))				{ edited.parameters[key] = value; _setDefaultParameters(edited); }	// emptied: the default
	else
	{
		Log::log() << "FactorsForm " << name() << ": unknown definition key '" << key << "'" << std::endl;
		return;
	}

	if (edited.variable == definition->variable && edited.specification == definition->specification && edited.parameters == definition->parameters && edited.restriction == definition->restriction)
	{
		if (isParameter(key) && value != edited.parameters.value(key))
		{
			_specificationsRevision++;	// e.g. emptied at its default: the view shows the default again
			emit specificationsChanged();
		}
		return;
	}

	if (conflicts(edited))
	{
		addControlWarningTemporary(tr("%1 is already a term: change its specification or remove it.").arg(_label(edited)));
		_specificationsRevision++;	// the view shows the stored value again
		emit specificationsChanged();
		return;
	}

	*definition = edited;	// its term keeps its place in the models under its new label (_definitionsEdited)
	_definitionsEdited();
}

void FactorsFormBase::addDefinition()
{
	// an empty row at the bottom: the user picks its variable, which gives it a specification
	TermDefinition definition;
	definition.id = "d" + QString::number(_nextDefinitionId++);
	_definitions.push_back(definition);
	_definitionsEdited();
}

void FactorsFormBase::resetDefinitions()
{
	// back to the plain variables, and the table back to the base definitions (the defined terms are forgotten)
	setAdvanced(false);
	_definitions.clear();
	_interceptRestriction.clear();
	_syncDefinitions();
}

void FactorsFormBase::removeDefinition(QString id)
{
	auto definition = std::find_if(_definitions.begin(), _definitions.end(), [&](const TermDefinition & candidate) { return candidate.id == id; });
	if (definition == _definitions.end())
		return;

	if (_isBase(*definition))
		return;	// every variable keeps its base definition

	_definitions.erase(definition);	// its term leaves every model with the available list
	_definitionsEdited();
}

// Switching the models --------------------------------------------------------------------------------------------------

void FactorsFormBase::setAdvancedAvailable(bool available)
{
	if (_advancedAvailable == available)
		return;

	const bool wasActive = advancedActive();
	_advancedAvailable = available;
	emit advancedAvailableChanged();

	if (wasActive != advancedActive())
		emit advancedActiveChanged();

	if (_advancedAvailable && _factorsModel)
	{
		_connectUniverse();
		_syncDefinitions();
	}
}

void FactorsFormBase::setAdvanced(bool advanced)
{
	if (_advanced == advanced)
		return;

	const bool wasActive	= advancedActive(),
			   willBeActive	= _advancedAvailable && advanced;

	// The models get their new terms while the available list still offers the old ones, so nothing is dropped as
	// unavailable in between, and while the specification is on, so a nested form does not pass the terms of one
	// model on to the next one meanwhile (going off, it nests the models again afterwards).
	if (wasActive != willBeActive && _factorsModel)
	{
		if (willBeActive)
		{
			_syncDefinitions();
			_advanced = advanced;
		}
		_switchModels(willBeActive);
	}

	_advanced = advanced;
	emit advancedChanged();

	if (wasActive != advancedActive())
	{
		emit advancedActiveChanged();	// the available list offers the defined terms, or the variables again
		emit advancedToggled();			// models are nested again when it goes off
		emit definitionTermsChanged();

		if (initialized())
			setBoundValue(createEngineValue());
	}
}

Terms FactorsFormBase::_basicTerms(const Terms & advancedTerms) const
{
	// Every term becomes the term of its variables: log(x) -> x, log(x) * z -> x * z; the intercept and offsets are no
	// terms there.
	Terms result;
	for (const Term & term : advancedTerms)
	{
		QStringList variables;
		columnTypeVec types;
		bool valid = true;
		for (const QString & label : term.components())
		{
			const TermDefinition * definition = _definitionWithLabel(label);
			if (!definition || isOffset(definition->specification) || variables.contains(definition->variable)) { valid = false; break; }
			variables.append(definition->variable);
			types.push_back(_typeOf(definition->variable));
		}
		if (valid && !variables.isEmpty() && !result.containsValue(Term(variables, types)))
			result.add(Term(variables, types));
	}
	return result;
}

Terms FactorsFormBase::_advancedTerms(const Terms & basicTerms) const
{
	// Every model starts with the intercept; every variable becomes its base definition.
	Terms result = initialModelTerms();

	for (const Term & term : basicTerms)
	{
		QStringList labels;
		columnTypeVec types;
		for (const QString & variable : term.components())
			for (const TermDefinition & definition : _definitions)
				if (definition.variable == variable)
				{
					labels.append(_label(definition));
					types.push_back(_typeOf(variable));
					break;
				}
		if (labels.size() == term.components().size())
			result.add(Term(labels, types));
	}
	return result;
}

void FactorsFormBase::_switchModels(bool toAdvanced)
{
	for (int i = 0; i < _factorsModel->count(); i++)
	{
		Factor		*	factor	= _factorsModel->factor(i);
		const Terms		current	= factor->terms();

		const Terms		next	= toAdvanced ? _advancedTerms(current) : _basicTerms(current);

		ListModelDraggable * model = factor->listView ? qobject_cast<ListModelDraggable *>(factor->listView->model()) : nullptr;
		if (model)	model->initTerms(next);
		else		factor->initTerms = next;
	}
}

// Option values ---------------------------------------------------------------------------------------------------------

///< The rows of `definitions`: R gets the parameters of each specification (integers as numbers), the file keeps every
/// parameter ever set (so switching back to a specification restores its settings) and the label. Unfinished rows are
/// left out of both.
Json::Value FactorsFormBase::_definitionsJson(bool engine, const std::vector<const TermDefinition *> & definitions, Json::Value * types) const
{
	Json::Value rows(Json::arrayValue);

	for (const TermDefinition * definition : definitions)
	{
		if (!_isComplete(*definition))
			continue;

		Json::Value row(Json::objectValue);
		row["id"]				= fq(definition->id);
		row["specification"]	= fq(definition->specification);

		if (definition->specification == "offsetConstant")	// no variable: a number
		{
			double value = 0;
			isFiniteNumber(definition->parameters.value("value"), &value);
			row["value"] = engine ? Json::Value(value) : Json::Value(fq(definition->parameters.value("value").trimmed()));
			if (!engine)
				row["label"] = fq(_label(*definition));
			rows.append(row);
			if (types)
				types->append(columnTypeToString(columnType::unknown));
			continue;
		}

		row["variable"]			= fq(definition->variable);

		for (const QString & parameter : engine ? parametersOf(definition->specification) : definition->parameters.keys())
		{
			const Parameter *	info	= parameterOf(definition->specification, parameter);
			QString				value	= definition->parameters.value(parameter);
			if (engine && info && info->type == "level" && value == _levels(definition->variable).value(0))
				value.clear();	// the default: R takes the first level of the rows it fits, which can differ from the column's
			row[fq(parameter)] = engine && info && info->type == "integer" ? Json::Value(value.toInt()) : Json::Value(fq(value));
		}

		if (!engine || (_restrictionAvailable && !isOffset(definition->specification)))	// an offset has no coefficient to restrict
			row["restriction"] = fq(definition->restriction);
		if (!engine)
			row["label"] = fq(_label(*definition));	// its term in the models, renamed on loading if a later version labels it differently

		rows.append(row);
		if (types)
			types->append(columnTypeToString(_typeOf(definition->variable)));
	}

	return rows;
}

///< Fills the Engine members of a model while the specification is on (see the file header). With `filterIds`, also the
/// dataset/filter of each component and definition, positional like the members: the one its variable comes from, else
/// `hostId` (the model list's).
void FactorsFormBase::_advancedEngineMembers(const Factor & factor, Json::Value & factorJson, Json::Value * filterIds, int hostId) const
{
	std::vector<const TermDefinition *>	used;
	std::vector<QStringList>			componentVariables;
	Json::Value							components(Json::arrayValue),
										componentTypes(Json::arrayValue),
										termRows(Json::arrayValue);
	bool								intercept = _interceptMode == "forced";

	for (const Term & term : factor.terms())
	{
		if (term.components().size() == 1 && term.components()[0] == interceptLabel)
		{
			intercept = intercept || _interceptMode == "optional";
			continue;
		}

		QStringList operands, variables;
		std::vector<const TermDefinition *> definitions;
		for (const QString & label : term.components())
			if (const TermDefinition * definition = _definitionWithLabel(label))
			{
				definitions.push_back(definition);
				operands.append(definition->id);
				if (!definition->variable.isEmpty() && !variables.contains(definition->variable))	// a constant offset has none
					variables.append(definition->variable);
			}

		if (definitions.size() != size_t(term.components().size()))
			continue;	// the intercept inside an interaction (removed right after), or a label no definition has any more

		for (const TermDefinition * definition : definitions)
			if (std::find(used.begin(), used.end(), definition) == used.end())
				used.push_back(definition);

		Json::Value row(Json::objectValue), operandIds(Json::arrayValue);
		for (const QString & operand : operands)
			operandIds.append(fq(operand));
		row["id"]		= fq("t" + QString::number(termRows.size() + 1));
		row["operands"]	= operandIds;
		termRows.append(row);

		if (!variables.isEmpty() && std::find(componentVariables.begin(), componentVariables.end(), variables) == componentVariables.end())
		{
			componentVariables.push_back(variables);
			Json::Value value(Json::arrayValue), types(Json::arrayValue);
			for (const QString & variable : variables)
			{
				value.append(fq(variable));
				types.append(columnTypeToString(_typeOf(variable)));
			}
			components.append(allowInteraction() ? value : value[0]);
			componentTypes.append(allowInteraction() ? types : types[0]);
		}
	}

	Json::Value componentsOption(Json::objectValue), definitionTypes(Json::arrayValue), definitionsOption(Json::objectValue);
	componentsOption["value"]		= components;
	componentsOption["types"]		= componentTypes;
	definitionsOption["value"]		= _definitionsJson(true, used, &definitionTypes);
	definitionsOption["types"]		= definitionTypes;
	definitionsOption["optionKey"]	= "variable";

	factorJson[fq(_optionKeyValue)]	= componentsOption;
	factorJson["definitions"]		= definitionsOption;
	factorJson["terms"]				= termRows;

	factorJson["intercept"] = intercept;	// also with interceptMode "none": a missing one would be read as an intercept
	if (_restrictionAvailable && intercept)
		factorJson["interceptRestriction"] = fq(_interceptRestriction);

	if (filterIds)
	{
		auto stamp = [&](const QString & variable) { const int id = variable.isEmpty() ? -1 : _filterIdOf(variable); return id >= 0 ? Json::Value(id) : hostId >= 0 ? Json::Value(hostId) : Json::Value(); };

		Json::Value componentIds(Json::arrayValue), definitionIds(Json::arrayValue);
		for (const QStringList & variables : componentVariables)	componentIds.append(stamp(variables[0]));	// one id per row, as a combined term has in basic mode
		for (const TermDefinition * definition : used)				definitionIds.append(stamp(definition->variable));
		(*filterIds)[fq(_optionKeyValue)]	= componentIds;
		(*filterIds)["definitions"]			= definitionIds;
	}
}

Json::Value FactorsFormBase::createEngineMeta() const
{
	Json::Value meta = BoundControlBase::createEngineMeta();

	if (!advancedActive() || !meta.get("shouldEncode", false).asBool())
		return meta;

	Json::Value encode(Json::objectValue);
	encode["shouldEncode"] = true;

	Json::Value perModel(Json::objectValue);
	perModel[fq(_optionKeyValue)]		= encode;
	perModel["definitions"]["variable"]	= encode;

	return perModel;
}

///< A definition row of the option (Storage or Engine shape); not a definition when its id is empty.
FactorsFormBase::TermDefinition FactorsFormBase::_definitionFromJson(const Json::Value & row)
{
	TermDefinition definition;
	if (!row.isObject())
		return definition;

	definition.id				= stringOr(row.get("id", ""), "");
	definition.variable			= stringOr(row.get("variable", ""), "");
	definition.specification	= stringOr(row.get("specification", ""), "");
	definition.restriction		= scalarAsString(row.get("restriction", ""));
	for (const std::string & member : row.getMemberNames())	// every parameter: a file keeps the ones of other specifications too
		if (isParameter(tq(member)))
			definition.parameters[tq(member)] = scalarAsString(row[member]);

	if (definition.specification == "offsetConstant")
		definition.variable.clear();	// a number, no variable

	return definition;
}

///< An Engine value of the term specification (from RPC) is refused when binding it would build another model than the
/// one asked for: a definition that is not a term (no id, an unknown specification or one the form does not offer for
/// the variable, no variable or number), two definitions with one label, one id for two different definitions, a
/// restriction the form does not have, a term whose operands are not all definitions of its model, an interaction in a
/// form without `allowInteraction`, an intercept the interceptMode does not allow, or models with different intercept
/// restrictions (the table has one). (A refused value is reported as a loading error; the form keeps its state.)
bool FactorsFormBase::_advancedJsonValid(const Json::Value & value) const
{
	std::map<std::string, Json::Value>	definitionsById;
	QMap<QString, QString>				idsByLabel;
	QString								interceptRestriction;
	bool								interceptRestrictionSeen = false;

	for (const Json::Value & factorJson : value)
	{
		if (!factorJson.isObject() || !factorJson.isMember("terms"))
			continue;	// basic, or a saved file (its definitions are read as they are)

		const bool intercept = boolOr(factorJson.get("intercept", true), true);
		if ((_interceptMode == "forced" && !intercept) || (_interceptMode == "none" && intercept))
			return false;
		if (intercept)
		{
			const QString restriction = scalarAsString(factorJson.get("interceptRestriction", "")).trimmed();	// absent: free
			if ((!_restrictionAvailable && !restriction.isEmpty()) || (interceptRestrictionSeen && restriction != interceptRestriction))
				return false;
			interceptRestriction		= restriction;
			interceptRestrictionSeen	= true;
		}

		const Json::Value	rows	= rowsOf(factorJson.get("definitions", Json::nullValue)),
							types	= factorJson["definitions"].isObject() ? factorJson["definitions"].get("types", Json::arrayValue) : Json::Value(Json::arrayValue);
		std::set<std::string> ids;
		for (Json::ArrayIndex i = 0; i < rows.size(); i++)
		{
			TermDefinition definition = _definitionFromJson(rows[i]);
			const Specification * specification = specificationNamed(definition.specification);
			if (definition.id.isEmpty() || !_isComplete(definition) || !specification || (!_restrictionAvailable && !definition.restriction.trimmed().isEmpty()))
				return false;

			if (specification->kind == "constant")
			{
				if (!_constantOffsetAllowed())
					return false;
			}
			else
			{
				columnType type = _typeOf(definition.variable);	// the form's own type; the value's only for a variable it does not know yet
				if (type == columnType::unknown && types.size() > i && types[i].isString())
					type = columnTypeFromString(types[i].asString(), columnType::unknown);
				if (type != columnType::unknown && !_allowedSpecifications(type).contains(definition.specification))
					return false;
			}

			_setDefaultParameters(definition);
			const QString label = _label(definition);
			if (idsByLabel.value(label, definition.id) != definition.id)
				return false;
			idsByLabel[label] = definition.id;

			const Json::Value & row = rows[i];
			const std::string id = fq(definition.id);
			auto known = definitionsById.find(id);
			if (known != definitionsById.end() && known->second != row)
				return false;
			definitionsById[id] = row;
			ids.insert(id);
		}

		if (!factorJson["terms"].isArray())
			return false;
		for (const Json::Value & term : factorJson["terms"])
		{
			const Json::Value	operandsJson	= term.isObject() ? term.get("operands", Json::nullValue) : Json::Value();
			const QStringList	operands		= stringList(operandsJson);
			if (operands.isEmpty() || operands.size() != (operandsJson.isArray() ? int(operandsJson.size()) : 1) || (operands.size() > 1 && !allowInteraction()))
				return false;	// none, one that is not a string, or an interaction the models cannot hold
			for (const QString & operand : operands)
				if (!ids.count(fq(operand)))
					return false;
		}
	}

	return true;
}

void FactorsFormBase::_bindAdvanced(Json::Value & value, ListModelFactorsForm::FactorVec & factors)
{
	const std::vector<const char *> advancedKeys = { "advanced", "definitions", "terms", "intercept", "interceptRestriction" };

	if (_advancedAvailable)
	{
		// The definitions: all of them in a saved file, those each model uses in an Engine value (RPC).
		std::vector<TermDefinition> definitions;
		QMap<QString, QString>		savedLabels;	// id -> the label a saved file gave the definition
		QString						interceptRestriction;
		bool flagged = false, advanced = false, engineShaped = false;
		int maxId = 0;

		for (const Json::Value & factorJson : value)
		{
			if (!factorJson.isObject())
				continue;
			if (factorJson.isMember("advanced"))	{ flagged = true; advanced = advanced || boolOr(factorJson["advanced"], false); }
			if (factorJson.isMember("terms"))		engineShaped = true;
			if (factorJson.isMember("interceptRestriction"))
				interceptRestriction = scalarAsString(factorJson["interceptRestriction"]);

			for (const Json::Value & row : rowsOf(factorJson.get("definitions", Json::nullValue)))
			{
				TermDefinition definition = _definitionFromJson(row);
				_setDefaultParameters(definition);	// a parameter the value does not give (RPC, an older file) takes its default
				if (definition.id.isEmpty() || definition.specification.isEmpty() || !_isComplete(definition)
					|| std::any_of(definitions.begin(), definitions.end(), [&](const TermDefinition & other) { return other.id == definition.id; }))
					continue;	// (the same definition in several models: read once)

				maxId = std::max(maxId, idNumber(definition.id));
				definitions.push_back(definition);
				if (row.isMember("label"))
					savedLabels[definition.id] = stringOr(row["label"], "");
			}
		}

		// "advanced" in a saved file; an Engine value is active by its `terms`; a basic value stays basic
		const bool active = flagged ? advanced : engineShaped;
		if (flagged || engineShaped)
			_interceptRestriction = interceptRestriction;	// absent: free

		if (engineShaped && !flagged)
		{
			// An Engine value (RPC) has only the definitions its models use: the others stay (a variable no model uses keeps
			// its base definition), in their order, unless the value redefines them (same id, label or settings).
			QSet<QString> placed, labels, signatures;
			for (const TermDefinition & definition : definitions)
			{
				labels.insert(_label(definition));
				signatures.insert(_signature(definition));
			}

			std::vector<TermDefinition> merged;
			for (const TermDefinition & current : _definitions)
			{
				auto replacement = std::find_if(definitions.begin(), definitions.end(), [&](const TermDefinition & definition) { return definition.id == current.id; });
				if (replacement != definitions.end())
				{
					merged.push_back(*replacement);
					placed.insert(current.id);
				}
				else if (!_isComplete(current) || (!labels.contains(_label(current)) && !signatures.contains(_signature(current))))
					merged.push_back(current);
			}
			for (const TermDefinition & definition : definitions)
				if (!placed.contains(definition.id))
					merged.push_back(definition);

			_definitions = merged;
			QMetaObject::invokeMethod(this, [this]() { _syncDefinitions(); }, Qt::QueuedConnection);	// variables the value does not mention
		}
		else if (!definitions.empty())
			_definitions = definitions;
		_nextDefinitionId = std::max(_nextDefinitionId, maxId + 1);
		_labels = _completeLabels();	// the labels the models hold from now on

		// A file saved by a version that labelled a definition differently: its terms get the current label.
		QMap<QString, QString> relabel;
		for (auto saved = savedLabels.begin(); saved != savedLabels.end(); ++saved)
			if (_labels.contains(saved.key()) && !saved.value().isEmpty() && saved.value() != _labels.value(saved.key()))
				relabel[saved.value()] = _labels.value(saved.key());

		for (size_t i = 0; i < factors.size(); i++)
		{
			const Json::Value & factorJson = value[Json::ArrayIndex(i)];
			if (!factorJson.isObject())
				continue;

			// An Engine value: the model's terms are its definitions' labels (its components are only variables)
			if (factorJson.isMember("terms"))
			{
				Terms terms;
				if (_interceptMode == "optional" && boolOr(factorJson.get("intercept", true), true))
					terms.add(Term(interceptLabel));

				for (const Json::Value & row : factorJson["terms"])
				{
					QStringList labels;
					columnTypeVec types;
					for (const QString & operand : stringList(row.get("operands", Json::arrayValue)))
						for (const TermDefinition & definition : _definitions)
							if (definition.id == operand)
							{
								labels.append(_label(definition));
								types.push_back(_typeOf(definition.variable));
							}
					if (!labels.isEmpty())
						terms.add(Term(labels, types));
				}
				factors[i].initTerms = terms;
			}

			for (auto it = relabel.begin(); it != relabel.end(); ++it)
				factors[i].initTerms.replaceVariableName(fq(it.key()), fq(it.value()));
		}

		const bool wasActive = advancedActive(), changed = _advanced != active;
		_advanced = active;
		if (changed)						emit advancedChanged();
		if (wasActive != advancedActive())	emit advancedActiveChanged();
		emit definitionTermsChanged();
	}

	// The bound value of the control is the Engine shape: the Storage-only keys go, the Engine members are regenerated
	for (size_t i = 0; i < factors.size(); i++)
	{
		Json::Value & factorJson = value[Json::ArrayIndex(i)];
		if (!factorJson.isObject())
			continue;
		for (const char * key : advancedKeys)
			factorJson.removeMember(key);

		if (advancedActive())
			_advancedEngineMembers(factors[i], factorJson);
	}

	_specificationsRevision++;
	emit specificationsChanged();
}

///< The intercept and offsets enter a model only on their own: an interaction made with one of them (e.g. by assigning
/// "(Intercept)" together with a variable, or by turning a definition into an offset) is removed again.
void FactorsFormBase::_removeInvalidInteractions()
{
	if (!advancedActive() || !_factorsModel)
		return;

	bool removed = false;
	for (int i = 0; i < _factorsModel->count(); i++)
	{
		Factor				*	factor	= _factorsModel->factor(i);
		ListModelDraggable	*	model	= factor->listView ? qobject_cast<ListModelDraggable *>(factor->listView->model()) : nullptr;
		if (!model)
			continue;

		QList<int> invalid;
		const Terms & terms = model->terms();
		for (size_t j = 0; j < terms.size(); j++)
			if (terms.at(j).components().size() > 1)
				for (const QString & component : terms.at(j).components())
				{
					const TermDefinition * definition = _definitionWithLabel(component);
					if (component == interceptLabel || (definition && isOffset(definition->specification)))
					{
						invalid.append(int(j));
						break;
					}
				}

		if (!invalid.isEmpty())
		{
			model->removeTerms(invalid);
			removed = true;
		}
	}

	if (removed)
		addControlWarningTemporary(tr("The intercept and offsets cannot be part of an interaction."));
}

// The terms of one model, for the full-width view of a model (FactorsFormModelView.qml) --------------------------------

QVariantList FactorsFormBase::modelTermRows(int factorIndex) const
{
	QVariantList rows;
	if (!_factorsModel || factorIndex < 0 || factorIndex >= _factorsModel->count())
		return rows;

	const Terms & terms = _factorsModel->factor(factorIndex)->terms();
	for (const Term & term : terms)
	{
		QVariantMap row;
		row["label"] = term.components().join(QString::fromUtf8(" ✻ "));
		rows.append(row);
	}

	return rows;
}

void FactorsFormBase::removeModelTerm(int factorIndex, int termIndex)
{
	if (!_factorsModel || factorIndex < 0 || factorIndex >= _factorsModel->count())
		return;

	Factor				*	factor	= _factorsModel->factor(factorIndex);
	ListModelDraggable	*	model	= factor->listView ? qobject_cast<ListModelDraggable *>(factor->listView->model()) : nullptr;
	if (model && termIndex >= 0 && termIndex < int(model->terms().size()))
		model->removeTerms({ termIndex });
}

void FactorsFormBase::_connectUniverse()
{
	if (_universeConnected || !_advancedAvailable || !_universeList || !_universeList->model())
		return;

	// Renamed columns and levels come from the data itself: the hidden list takes the new names as new terms (from the
	// lists it is fed by) before it would hear of a rename, so it never reports one. The data's signal arrives after the
	// lists took the new names, and the sync is queued, so the definitions are renamed before it runs (else it would
	// replace them by new ones).
	_universeConnected = true;
	connect(_universeList->model(), &ListModel::termsChanged, this, &FactorsFormBase::_syncDefinitions, Qt::QueuedConnection);
	if (VariableInfo * data = _universeList->resolvedVarInfo())
	{
		connect(data, &VariableInfo::variableNamesChanged,	this, &FactorsFormBase::_variablesRenamed);
		connect(data, &VariableInfo::labelsChanged,			this, &FactorsFormBase::_levelsRenamed);
		connect(data, &VariableInfo::labelsReordered,		this, [this]() { _definitionsEdited(); });	// the first level can change
	}

	auto checkNames = [&](const QStringList & names, const QStringList & kinds)
	{
		for (const QString & name : names)
			if (name != "default" && (!specificationNamed(name) || !kinds.contains(specificationNamed(name)->kind)))
				Log::log() << "FactorsForm " << this->name() << ": specification '" << name << "' ignored (unknown, or not of this list)" << std::endl;
	};
	checkNames(_continuousSpecifications, { "continuous", "constant" });
	checkNames(_categoricalSpecifications, { "categorical" });

	_syncDefinitions();
}
