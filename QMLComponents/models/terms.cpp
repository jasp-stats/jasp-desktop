//
// Copyright (C) 2013-2025 University of Amsterdam
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
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "terms.h"

#include <QSet>
#include "qutils.h"
#include "variableinfo.h"
#include "log.h"

using namespace std;

// a term composed out of others (cross/way/interaction combination) inherits the
//source filter only when ALL contributing terms agree on one — the components of an interaction
//share the term-level filterId. Mixed provenance cannot be expressed by a single
//term-level id, so a mixed combination stays unqualified (-1) rather than lying about one source.
namespace
{
	constexpr int kNoFilterIdYet = -2;		//running-state sentinels for the carry below, never term values
	constexpr int kMixedFilterIds = -3;

	void carryFilterId(int &soFar, int candidate)
	{
		if (soFar == kMixedFilterIds)
			return;
		if (soFar == kNoFilterIdYet)
			soFar = candidate;
		else if (soFar != candidate)
			soFar = kMixedFilterIds;
	}

	int settledFilterId(int soFar)
	{
		return soFar == kMixedFilterIds || soFar == kNoFilterIdYet ? -1 : soFar;
	}
}

Terms::Terms(const QList<QString> &terms, Terms *parent)
{
	_parent = parent;
	set(terms);
}

Terms::Terms(const std::vector<std::vector<string> > &terms, Terms *parent)
{
	_parent = parent;
	set(terms);
}

Terms::Terms(const QList<Term> &terms, Terms *parent)
{
	_parent = parent;
	set(terms);
}

Terms::Terms(Terms *parent)
{
	_parent = parent;
}

// the ONE options-json -> Terms applier (see the header). Absorbs the bind-time
//shape normalization into this chokepoint:
//  * `{value, types}` option objects are unwrapped (a bare string/array value parses too - that
//    is what R-syntax-simplified and pre-{value,types} files carry),
//  * a single-string `types` becomes a one-element array (pre-0.19 files),
//  * an interaction list WITHOUT row components accepts the simplified `[value, [c1, c2]]` form:
//    bare entries become `{keyValue: [components]}` rows, exactly the shape Term(json) parses.
//    Like the deleted helper, the wrap only applies to a bare ARRAY option value (a stored
//    `{value: [...]}` keeps its rows as they are) and object rows pass through untouched.
Terms::Terms(const Json::Value &optionValue, const std::string &keyValue, const std::string &keyLabel, RelatedValuesPerTerm &allControlValues, bool containsInteractions, bool hasRowComponent)
{
	//Provenance rides the wire only in the Storage shape (see the header):
	//ENGINE-shape and released-file terms parse UNQUALIFIED (-1) exactly as before, the list they
	//land in stamps the current selection in (ListModel::_setAllowedType, only onto
	//terms that come in unqualified), bindTo-copied typed Terms keep their own ids and the
	//name-only wildcard lookup matches the freshly parsed unqualified terms against the stamped model.
	Json::Value valuesPart	= optionValue;
	Json::Value typesPart	= Json::arrayValue;


	if (valuesPart.isObject() && valuesPart.isMember("value") && valuesPart.isMember("types"))
	{
		typesPart = valuesPart["types"];
		valuesPart = valuesPart["value"];
	}
	else if (valuesPart.isObject() && valuesPart.isMember("value") && !valuesPart.isMember("types"))
	{
		// the Storage option object `{value, optionKey?}` - types live INSIDE the
		//per-term objects now. No legacy path ever produced a `{value}`-without-`types` option
		//object (BoundControlTerms::isJsonValid always rejected it), so this unwrap is
		//strictly the new file shape; `{value, types}` keeps taking the engine branch above.
		valuesPart = valuesPart["value"];
	}

	if (typesPart.isString())
	{
		Json::Value typesAsArray(Json::arrayValue);
		typesAsArray.append(typesPart);
		typesPart = typesAsArray;
	}

	if (!hasRowComponent && containsInteractions && optionValue.isArray())
	{
		Json::Value rows(Json::arrayValue);
		for (const Json::Value& aValue : optionValue)
		{
			if (aValue.isObject())
				rows.append(aValue);
			else
			{
				Json::Value row(Json::objectValue);
				Json::Value components(Json::arrayValue);
				if (aValue.isString())
					components.append(aValue);
				else if (aValue.isArray())
				{
					for (const Json::Value& comp : aValue)
					{
						if (comp.isString())
							components.append(comp);
						else
							Log::log() << "Wrong Json type when binding interaction terms: " << optionValue.toStyledString() << std::endl;
					}
				}
				else
					Log::log() << "Wrong Json type when binding interaction terms: " << optionValue.toStyledString() << std::endl;

				row[keyValue] = components;
				rows.append(row);
			}
		}
		valuesPart = rows;
	}

	if (valuesPart.isString())
	{
		std::string str = valuesPart.asString();
		columnType type = columnType::unknown;

		if (typesPart.isString())
			type = columnTypeFromString(typesPart.asString(), columnType::unknown);
		else if (typesPart.isArray() && typesPart.size() > 0 && typesPart[0].isString())
			type = columnTypeFromString(typesPart[0].asString(), columnType::unknown);

		if (!str.empty())
		{
			// a single-string option value that IS a qualified surface form
			// resolves into the typed term - ids only, the ("Type") segment
			//wins over the positional/lookup type when present. Bare values keep today's
			//path byte-identically (their fallback is the caller's context).
			const QualifiedTermComponent resolved = resolveQualifiedTermComponent(tq(str));
			Term term(fq(resolved.bareName), type);
			if (resolved.qualified)
			{
				term.setFilterId(resolved.filterId);
				if (resolved.hasTypeSegment)
					term.setType(resolved.type);
			}
			add(term);
		}
	}
	else if (Term::isStorageTermObject(valuesPart))
		// single-row Storage option - one term object as the value.
		add(Term::fromStorageTermObject(valuesPart));
	else
	{
		int termId = 0;

		for (const Json::Value& row : valuesPart)
		{
			//Only strings and objects are valid term rows; anything else (numbers,
			//bools, nulls from malformed input) is skipped instead of throwing.
			if (!row.isString() && !row.isObject())
			{
				termId++;
				continue;
			}

			columnTypeVec types;
			if (typesPart.size() > termId)
			{
				if (typesPart[termId].isArray())
					for (const Json::Value& jsonType : typesPart[termId])
						types.push_back(columnTypeFromString(jsonType.asString(), columnType::unknown));
				else
					types.push_back(columnTypeFromString(typesPart[termId].asString(), columnType::unknown));
			}

			// the entry dispatch. A Storage term object `{name, filterId?, types}`
			//parses to a TYPED term (its filterId included - the flagship x@11/x@12 twins come
			//back as two distinct Terms and Terms::add keeps both, strict equality);
			//every other entry keeps taking today's byte-identical path (bare string, or a row
			//object whose keyValue entry may itself be a nested Storage term object, see
			//Term(json)). The strict member test in isStorageTermObject is what keeps rows and
			//factor-level rows out of the new branch.
			const bool isTermObject = row.isObject() && Term::isStorageTermObject(row);

			Term term = [&]()
			{
				if (row.isString())
				{
					// a string row that IS a qualified surface form
					//resolves into the typed term - the bare string rows around it keep today's
					//path (and today's -1, which the list stamps with its host on intake).
					const QualifiedTermComponent resolved = resolveQualifiedTermComponent(tq(row.asString()));
					Term parsed(fq(resolved.bareName), types);
					if (resolved.qualified)
					{
						parsed.setFilterId(resolved.filterId);
						if (resolved.hasTypeSegment)	//the ("Type") segment wins over the positional type
							parsed.setType(resolved.type);
					}
					return parsed;
				}

				return isTermObject ? Term::fromStorageTermObject(row, types)
									: Term(row, keyValue, keyLabel, types);
			}();
			if (term.size() > 0)
			{
				add(term);

				//A term object's members ARE the term, not row children - collecting them as
				//related values would fabricate `types`/`filterId` row controls out of the format.
				if (row.isObject() && !isTermObject)
				{
					QMap<QString, Json::Value> controlMap;
					for (auto itr = row.begin(); itr != row.end(); ++itr)
					{
						const std::string& name = itr.key().asString();
						if (name != keyValue)
							controlMap[tq(name)] = *itr;
					}
					// keyed by ROW IDENTITY - twin rows parsed from the Storage shape
					//carry their own filterId and now keep their OWN children; the -1 stamped
					//later onto engine-shaped parses stays findable through relatedValuesForTerm()'s
					//fallback. (Two same-name rows in one ENGINE tree still share the single -1
					//entry - the wire carries no provenance to tell them apart.)
					allControlValues[TermRowKey::ofTerm(term)] = controlMap;
				}
			}

			termId++;
		}
	}
}

void Terms::set(const std::vector<Term> &terms, bool isUnique)
{
	_terms.clear();
	_valueMap.clear();

	for(const Term &term : terms)
		add(term, isUnique);
}

void Terms::set(const std::vector<std::vector<string> > &terms, bool isUnique)
{
	_terms.clear();
	_valueMap.clear();

	for(const Term &term : terms)
		add(term, isUnique);

}

void Terms::set(const QList<Term> &terms, bool isUnique)
{
	_terms.clear();
	_valueMap.clear();

	for(const Term &term : terms)
		add(term, isUnique);

}

void Terms::set(const Terms &terms, bool isUnique)
{
	_terms.clear();
	_valueMap.clear();
	_hasDuplicate = terms.hasDuplicate();

	for(const Term &term : terms)
		add(term, isUnique);

}

void Terms::set(const QList<QString> &terms, bool isUnique)
{
	_terms.clear();
	_valueMap.clear();

	for(const QString &value : terms)
		add(Term(value), isUnique);

}

void Terms::setSortParent(const Terms &parent)
{
	_parent = &parent;
}

void Terms::removeParent() {
	_parent = nullptr;
}

void Terms::add(const Term &term, bool isUnique)
{
	if (!isUnique || _hasDuplicate)
	{
		if (!_hasDuplicate && containsValue(term)) _hasDuplicate = true;
		_terms.push_back(term);
		_valueMap[term.value()] = _terms.size() - 1;
	}
	else if (_parent != nullptr)
	{
		vector<Term>::iterator itr = _terms.begin();
		int result = -1;
		int index = 0;

		for (; itr != _terms.end(); itr++)
		{
			result = termCompare(term, *itr);
			if (result >= 0)
				break;
			index++;
		}

		if (result > 0)
		{
			itr = _terms.insert(itr, term);
			for (; itr != _terms.end(); itr++)
				_valueMap[itr->value()] = index++;
		}
		else if (result == 0)
		{
			itr->setDraggable(term.isDraggable());
			itr->setTypes(term.types());
		}
		else if (result < 0)
		{
			_terms.push_back(term);
			_valueMap[term.value()] = _terms.size() - 1;
		}
	}
	else
	{
		int index = indexOfValue(term);
		if (index < 0)
		{
			_terms.push_back(term);
			_valueMap[term.value()] = _terms.size() - 1;
		}
		else
		{
			_terms.at(index).setDraggable(term.isDraggable());
			_terms.at(index).setTypes(term.types());
		}
	}
}

void Terms::insert(int index, const Term &term)
{
	if (_parent == nullptr)
	{
		if (index > _terms.size())
			index = _terms.size();

		vector<Term>::iterator itr = std::next(_terms.begin(), index);

		itr = _terms.insert(itr, term);
		for (; itr != _terms.end(); itr++)
			_valueMap[itr->value()] = index++;
	}
	else
		add(term);
}

void Terms::insert(int index, const Terms &terms)
{
	for (const Term& term : terms)
		insert(index++, term);
}

void Terms::add(const Terms &terms)
{
	_hasDuplicate = _hasDuplicate || terms.hasDuplicate();

	for(const Term & term : terms)
		add(term);
}

const Term& Terms::at(size_t index) const
{
	return _terms.at(index);
}

Term &Terms::at(size_t index)
{
	return _terms.at(index);
}

bool Terms::containsValue(const Term &term) const
{
	return indexOfValue(term) >= 0;
}

bool Terms::containsValue(const QString &value) const
{
	return _valueMap.find(value) != _valueMap.end();
}

int Terms::indexOfValue(const Term &term) const
{
	// lookup rule: an UNQUALIFIED lookup term (-1) matches on name alone — the
	//context fallback ("this list holds that column, whichever filter it came through"): that is
	//exactly how freshly parsed/bound terms (-1, no wire field yet) still find their stamped twins
	//in a list. A STAMPED lookup term must match name AND filter — the same-name-different-filter
	//distinction. Equality itself stays strict (see Term::operator==); the wildcard is here, in
	//the lookup, never in ==.
	if (term.filterId() < 0)
		return indexOfValue(term.value());

	auto itr = _valueMap.find(term.value());
	if(itr != _valueMap.end() && _terms.at(size_t(itr->second)) == term)	//fast path: the name's map index already IS the stamped twin
		return itr->second;

	int index = 0;															//collisions are rare: scan only when the map entry mismatches
	for(const Term &candidate : _terms)
	{
		if (candidate == term)
			return index;
		++index;
	}
	return -1;
}

int Terms::indexOfValue(const QString &value) const
{
	auto itr = _valueMap.find(value);
	return (itr == _valueMap.end()) ? -1 : itr->second;
}

int Terms::indexOfLabel(const QString &label) const
{
	int i = 0;
	for(const Term &term : _terms)
	{
		if (term.label() == label)
			return i;
		i++;
	}

	return -1;
}

vector<string> Terms::valuesAsVector() const
{
	vector<string> items;

	for(const Term &term : _terms)
		items.push_back(fq(term.value()));

	return items;
}

vector<vector<string> > Terms::asVectorOfVectors() const
{
	vector<vector<string> > items;

	for(const Term &term : _terms)
	{
		vector<string> components = term.scomponents();
		items.push_back(components);
	}

	return items;
}

QStringList Terms::values() const
{
	QStringList items;

	for(const Term &term : _terms)
		items.append(term.value());

	return items;
}

QStringList Terms::labels() const
{
	QStringList items;

	for(const Term &term : _terms)
		items.append(term.label());

	return items;
}

Terms Terms::crossCombinations() const
{
	if (_terms.size() <= 1)
		return *this;

	Terms t;

	for (uint r = 1; r <= _terms.size(); r++)
	{
		vector<bool> v(_terms.size());
		fill(v.begin() + r, v.end(), true);

		do {

			vector<string> combination;
			columnTypeVec types;
			int carriedFilterId = kNoFilterIdYet;

			for (uint i = 0; i < _terms.size(); i++) {
				if (!v[i])
				{
					vector<string> components = _terms.at(i).scomponents();
					columnTypeVec termTypes = _terms.at(i).types();
					combination.insert(combination.end(), components.begin(), components.end());
					types.insert(types.end(), termTypes.begin(), termTypes.end());
					carryFilterId(carriedFilterId, _terms.at(i).filterId());
				}
			}

			Term newTerm(combination);
			newTerm.setTypes(types);
			newTerm.setFilterId(settledFilterId(carriedFilterId));
			t.add(newTerm);

		} while (std::next_permutation(v.begin(), v.end()));
	}

	return t;
}

Terms Terms::wayCombinations(int ways) const
{
	Terms t;

	for (int r = ways; r <= ways; r++)
	{
		vector<bool> v(_terms.size());
		std::fill(v.begin() + r, v.end(), true);

		do {

			vector<string> combination;
			columnTypeVec types;
			int carriedFilterId = kNoFilterIdYet;

			for (uint i = 0; i < _terms.size(); ++i) {
				if (!v[i])
				{
					vector<string> components = _terms.at(i).scomponents();
					columnTypeVec termTypes = _terms.at(i).types();
					combination.insert(combination.end(), components.begin(), components.end());
					types.insert(types.end(), termTypes.begin(), termTypes.end());
					carryFilterId(carriedFilterId, _terms.at(i).filterId());
				}
			}

			Term newTerm(combination);
			newTerm.setTypes(types);
			newTerm.setFilterId(settledFilterId(carriedFilterId));
			t.add(newTerm);

		} while (std::next_permutation(v.begin(), v.end()));
	}

	return t;
}

Terms Terms::ffCombinations(const Terms &terms)
{
	// full factorial combinations

	Terms combos = terms.crossCombinations();

	Terms newTerms;

	newTerms.add(*this);
	newTerms.add(combos);

	for (uint i = 0; i < _terms.size(); i++)
	{
		for (uint j = 0; j < combos.size(); j++)
		{
			QStringList termComponents = _terms.at(i).components();
			termComponents.append(combos.at(j).components());
			columnTypeVec types = _terms.at(i).types();
			columnTypeVec coTypes = combos.at(j).types();
			types.insert(types.end(), coTypes.begin(), coTypes.end());

			int carriedFilterId = kNoFilterIdYet;						// this-term and its
			carryFilterId(carriedFilterId, _terms.at(i).filterId());		//combination part must agree
			carryFilterId(carriedFilterId, combos.at(j).filterId());		//for the product to be qualified

			Term newTerm(termComponents);
			newTerm.setTypes(types);
			newTerm.setFilterId(settledFilterId(carriedFilterId));
			newTerms.add(newTerm);
		}
	}

	newTerms.add(terms);

	return newTerms;
}

Terms Terms::combineTerms(JASPControl::CombinationType type)
{
	Terms combinedTerms;

	size_t nbTerms = size();

	switch (type)
	{
	case JASPControl::CombinationType::CombinationCross:
		combinedTerms = crossCombinations();
		break;
	case JASPControl::CombinationType::CombinationInteraction:
		combinedTerms = wayCombinations(nbTerms);
		break;
	case JASPControl::CombinationType::Combination2Way:
		combinedTerms = nbTerms < 2 ? Terms() : wayCombinations(2);
		break;
	case JASPControl::CombinationType::Combination3Way:
		combinedTerms = nbTerms < 3 ? Terms() : wayCombinations(3);
		break;
	case JASPControl::CombinationType::Combination4Way:
		combinedTerms = nbTerms < 4 ? Terms() : wayCombinations(4);
		break;
	case JASPControl::CombinationType::Combination5Way:
		combinedTerms = nbTerms < 5 ? Terms() : wayCombinations(5);
		break;
	case JASPControl::CombinationType::NoCombination:
	default:
		combinedTerms = *this;
		break;
	}

	return combinedTerms;
}

bool Terms::operator==(const Terms &terms) const
{
	return _terms == terms._terms;
}

bool Terms::operator!=(const Terms &terms) const
{
	return _terms != terms._terms;
}

bool Terms::strictlyEquals(const Terms &terms) const
{
	bool isEqual = _terms == terms._terms;

	for (size_t i = 0; isEqual && (i < _terms.size()); i++)
		isEqual = _terms.at(i).isDraggable() == terms._terms.at(i).isDraggable();

	return isEqual;
}

void Terms::setDraggable(bool draggable)
{
	for (Term& term : _terms)
		term.setDraggable(draggable);
}

void Terms::setUndraggableTerms(const Terms& undraggableTerms)
{
	std::vector<Term> newTerms = undraggableTerms._terms;
	for (Term& term : newTerms)
		term.setDraggable(false);

	// Then add only the draggabled terms that are not in undraggableTerms and that are draggable
	// All undraggable terms that are not in undraggableTerms will be then automatically removed.
	for (Term term : _terms)
	{
		if (term.isDraggable() && !undraggableTerms.containsValue(term))
			newTerms.push_back(term);
	}

	set(newTerms);
}

Json::Value Terms::types(bool onlyChanged, const VariableInfoConsumer* info) const
{
	Json::Value types(Json::arrayValue);

	auto changedType = [&, onlyChanged, info] (const QString& variable, columnType type) -> Json::Value
	{
		if (onlyChanged && info && (columnType)info->requestInfo(varInfoType::VariableType, variable).toInt() == type)
			return Json::nullValue;
		else
			return columnTypeToString(type);
	};

	for (const Term& term : _terms)
	{
		if (term.components().size() == 1)
			types.append(changedType(term.value(), term.type()));
		else
		{
			Json::Value componentTypes(Json::arrayValue);
			columnTypeVec termTypes = term.types();
			int i = 0;
			for (const QString component : term.components())
			{
				componentTypes.append(changedType(component, termTypes.size() > i ? termTypes[i] : columnType::unknown));
				i++;
			}
			types.append(componentTypes);
		}
	}

	return types;
}

Json::Value Terms::getValuesOptions() const
{
	Json::Value options(Json::arrayValue);
	for (const Term& term : _terms)
		options.append(fq(term.value()));

	return options;
}

// the row-identity lookup for RelatedValuesPerTerm (see the header). Exact
//(value, filterId) hit first; a stamped term whose row children were loaded UNQUALIFIED (the
//JSON applier parses engine-shape input with filterId -1, and ListModel::_setAllowedType stamps
//the host afterwards - the bind flow must still find them) falls back to that (value, -1) entry.
//A term never sees a DIFFERENT twin's children: that cross-talk is precisely what the keying fix
//removes. Unqualified terms keep the historical name-only behavior (their exact key IS (name,-1)).
const QMap<QString, Json::Value> & Terms::relatedValuesForTerm(const Terms::RelatedValuesPerTerm & values, const Term & term)
{
	static const QMap<QString, Json::Value> emptyValues;

	auto found = values.find(TermRowKey::ofTerm(term));
	if (found != values.end())
		return found.value();

	if (term.filterId() >= 0)
	{
		found = values.find(TermRowKey(term.value(), -1));
		if (found != values.end())
			return found.value();
	}

	return emptyValues;
}

// the row-with-related-values assembly lives HERE (the separate shuffle of the
//mirror era is deleted; ComponentsList/InputList row builders go
//through this one serialization too). The row object is the on-demand aggregation of the
//term's child values: keyValue (+ keyLabel when the list uses one) plus every related value
//the model holds for the term - which is exactly what the form-wide generator then merges the
//live row-child control values into, one implementation for both paths.
// this is the ENGINE serialization (the frozen R contract + the send-time
//shape): the per-term filterId deliberately does NOT appear here — on the run wire provenance
//rides the transient `.filterIds` sidecar only, and in the FILE it rides inside the
//per-term objects of the Storage shape (getStorageOptions below).
// the related values are looked up by ROW IDENTITY (value + filterId, with the
//-1 fallback), so same-name rows behind different filters each aggregate their OWN children.
Json::Value Terms::getOptions(const Terms::RelatedValuesPerTerm& relatedValuesPerTerm, const std::string& keyValue, const std::string& keyLabel, bool containsInteractions, bool hasRowComponent, bool isSingleRow, bool useValueAndType) const
{
	Json::Value result(Json::objectValue);

	Json::Value optionValue;

	if (hasRowComponent || containsInteractions)
	{
		Json::Value rows(Json::arrayValue);

		for (const Term& term : _terms)
		{
			const QMap<QString, Json::Value>& relatedValues = Terms::relatedValuesForTerm(relatedValuesPerTerm, term);

			Json::Value rowValues(Json::objectValue);

			rowValues[keyValue] = term.toJson(containsInteractions, useValueAndType);
			if (!keyLabel.empty())
				rowValues[keyLabel] = fq(term.label());

			QMapIterator<QString, Json::Value> it2(relatedValues);
			while (it2.hasNext())
			{
				it2.next();
				rowValues[fq(it2.key())] = it2.value();
			}
			rows.append(rowValues);
		}

		optionValue = rows;
	}
	else if (isSingleRow)
		optionValue = _terms.size() > 0 ? fq(_terms[0].value()) : "";
	else
		optionValue = getValuesOptions();

	result["value"] = optionValue;
	result["types"] = types();

	if (hasRowComponent || containsInteractions)
		result["optionKey"] = keyValue;

	return result;
}

// THE Storage serialization (see the header). Same row/value discipline as
//getOptions - optionKey kept, keyLabel kept, related child values keep today's row shape - but
//each term entry is the clean typed object {name, filterId?, types} and there is no outer
//positional `types` array (types live per term; the positional alignment of the Engine wire is
//frozen and untouched, landmine honored: this tree never reaches the engine, the applier parses
//it back into typed Terms and the Engine generator rebuilds the sidecar from THOSE).
//Empty cases mirror the engine shape ("value": [] / "") so parse->regenerate is byte-equal for
//every family (pinned by testStorageTermShapeAndRoundTrip).
Json::Value Terms::getStorageOptions(const Terms::RelatedValuesPerTerm& relatedValuesPerTerm, const std::string& keyValue, const std::string& keyLabel, bool containsInteractions, bool hasRowComponent, bool isSingleRow) const
{
	Json::Value result(Json::objectValue);

	Json::Value optionValue;

	if (hasRowComponent || containsInteractions)
	{
		Json::Value rows(Json::arrayValue);

		for (const Term& term : _terms)
		{
			const QMap<QString, Json::Value>& relatedValues = Terms::relatedValuesForTerm(relatedValuesPerTerm, term);	// row identity, see getOptions

			Json::Value rowValues(Json::objectValue);

			rowValues[keyValue] = term.toJsonStorage();
			if (!keyLabel.empty())
				rowValues[keyLabel] = fq(term.label());

			QMapIterator<QString, Json::Value> it2(relatedValues);
			while (it2.hasNext())
			{
				it2.next();
				rowValues[fq(it2.key())] = it2.value();
			}
			rows.append(rowValues);
		}

		optionValue = rows;
	}
	else if (isSingleRow)
		optionValue = _terms.size() > 0 ? _terms[0].toJsonStorage() : Json::Value("");
	else
	{
		Json::Value terms(Json::arrayValue);
		for (const Term& term : _terms)
			terms.append(term.toJsonStorage());
		optionValue = terms;
	}

	result["value"] = optionValue;

	if (hasRowComponent || containsInteractions)
		result["optionKey"] = keyValue;

	return result;
}


int Terms::rankOf(const QString &component) const
{
	if (_parent == nullptr)
		return 0;

	int index = 0;

	for(const Term& compare : _parent->terms())
	{
		if (compare.label() == component)
			break;
		index++;
	}

	return index;
}

int Terms::termCompare(const Term &t1, const Term &t2) const
{
	if (_parent == nullptr)
		return 1;

	if (t1.size() < t2.size())
		return 1;
	if (t1.size() > t2.size())
		return -1;

	for (uint i = 0; i < t1.size(); i++)
	{
		int t1Rank = rankOf(t1.at(i));
		int t2Rank = rankOf(t2.at(i));

		if (t1Rank < t2Rank)
			return 1;
		if (t1Rank > t2Rank)
			return -1;
	}

	return 0;
}

bool Terms::termLessThan(const Term &t1, const Term &t2) const
{
	return termCompare(t1, t2) > 0;
}

bool Terms::componentLessThan(const QString &c1, const QString &c2) const
{
	return rankOf(c1) < rankOf(c2);
}

void Terms::remove(const Terms &terms)
{
	for(const Term &term : terms)
	{
		vector<Term>::iterator itr = find(_terms.begin(), _terms.end(), term);
		if (itr != _terms.end())
		{
			int index = std::distance(_terms.begin(), itr);
			itr = _terms.erase(itr);
			_valueMap.erase(term.value());
			for (; itr != _terms.end(); itr++)
				_valueMap[itr->value()] = index++;
		}
	}
}

void Terms::remove(size_t pos, size_t n)
{
	vector<Term>::iterator itr = _terms.begin();

	for (size_t i = 0; i < pos && itr != _terms.end(); i++)
		itr++;

	for (; n > 0 && itr != _terms.end(); n--)
	{
		_valueMap.erase(itr->value());
		itr = _terms.erase(itr);
	}

	for (; itr != _terms.end(); itr++)
		_valueMap[itr->value()] = pos++;
}

void Terms::replace(int pos, const Term &term)
{
	size_t pos_t = size_t(pos);
	if (pos_t <_terms.size())
	{
		remove(pos_t);
		insert(pos, term);
	}
}

bool Terms::discardWhatDoesContainTheseComponents(const Terms &terms)
{
	bool changed = false;

	_terms.erase(
		std::remove_if(
			_terms.begin(),
			_terms.end(),
			[&](Term& existingTerm)
			{
				for (const Term &term : terms)
					for (const QString &component : term.components())
						if (existingTerm.contains(component))
						{
							changed	= true;
							return true;
						}


				return false;
			}),
		_terms.end()
	);

	if (changed)
		resetValueMap();

	return changed;
}

bool Terms::discardWhatDoesNotContainTheseComponents(const Terms &terms)
{
	bool changed = false;

	_terms.erase(
		std::remove_if(
			_terms.begin(),
			_terms.end(),
			[&](Term& existingTerm)
			{
				for (const QString &component : existingTerm.components())
					if (!terms.containsValue(component))
					{
						changed	= true;
						return true;
					}

				return false;
			}),
		_terms.end()
		);

	if (changed)
		resetValueMap();

	return changed;
}


bool Terms::discardWhatDoesContainTheseTerms(const Terms &terms)
{
	bool changed = false;

	_terms.erase(
		std::remove_if(
			_terms.begin(),
			_terms.end(),
			[&](const Term& existingTerm)
			{
				for (const Term &term : terms)
					if (existingTerm.containsAll(term))
					{
						changed = true;
						return true;
					}

				return false;
			}),
		_terms.end()
	);

	if (changed)
		resetValueMap();

	return changed;
}

bool Terms::discardWhatIsntTheseTerms(const Terms &terms, Terms *discarded)
{
	bool changed = false;

	_terms.erase(
		std::remove_if(
			_terms.begin(),
			_terms.end(),
			[&](Term& term)
			{
				//Name-based on purpose: the set partition of the row-level sync
				//flows ("is that entry still part of this other list"), where stamps take no
				//part - the origin-authoritative staleness logic lives pool-side (see
				//ListModelTermsAvailable::removeTermsInAssignedList), not here.
				if (!term.value().isEmpty() && !terms.containsValue(term.value()))
				{
					if (discarded != nullptr)
						discarded->add(term);

					changed = true;
					return true;
				}

				return false;
			}),
		_terms.end()
		);

	if (changed)
		resetValueMap();

	return changed;
}

void Terms::clear()
{
	_terms.clear();
	_valueMap.clear();
}

void Terms::resetValueMap()
{
	_valueMap.clear();
	int index = 0;

	for (const Term& term : _terms)
		_valueMap[term.value()] = index++;
}

size_t Terms::size() const
{
	return _terms.size();
}

const std::vector<Term> &Terms::terms() const
{
	return _terms;
}

Terms::const_iterator Terms::begin() const
{
	return _terms.begin();
}

Terms::const_iterator Terms::end() const
{
	return _terms.end();
}

Terms::iterator Terms::begin()
{
	return _terms.begin();
}

Terms::iterator Terms::end()
{
	return _terms.end();
}

void Terms::remove(const Term &term)
{
	vector<Term>::iterator itr = std::find(_terms.begin(), _terms.end(), term);
	if (itr != end())
	{
		int index = std::distance(_terms.begin(), itr);
		_valueMap.erase(itr->value());
		itr = _terms.erase(itr);

		for (; itr != _terms.end(); itr++)
			_valueMap[itr->value()] = index++;
	}
}

QSet<int> Terms::replaceVariableName(const std::string & oldValue, const std::string & newValue)
{
	QSet<int> change;

	int i = 0;
	for(Term & t : _terms)
	{
		QString oldTermValue = t.value();
		if (t.replaceVariableName(oldValue, newValue))
		{
			change.insert(i);
			_valueMap.erase(oldTermValue);
			_valueMap[t.value()] = i;
		}
		i++;
	}

	return change;
}
