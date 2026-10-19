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

using namespace std;

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

Terms::Terms(const Json::Value &valuesPart, const Json::Value &typesPart, const std::string &keyValue, const std::string &keyLabel, RelatedValuesPerTerm &allControlValues)
{
	if (valuesPart.isString())
	{
		std::string str = valuesPart.asString();
		columnType type = columnType::unknown;

		if (typesPart.isString())
			type = columnTypeFromString(typesPart.asString(), columnType::unknown);
		else if (typesPart.isArray() && typesPart.size() > 0 && typesPart[0].isString())
			type = columnTypeFromString(typesPart[0].asString(), columnType::unknown);

		if (!str.empty())
			add(Term(str, type));
	}
	else
	{
		int termId = 0;

		for (const Json::Value& row : valuesPart)
		{
			columnTypeVec types;
			if (typesPart.size() > termId)
			{
				if (typesPart[termId].isArray())
					for (const Json::Value& jsonType : typesPart[termId])
						types.push_back(columnTypeFromString(jsonType.asString(), columnType::unknown));
				else
					types.push_back(columnTypeFromString(typesPart[termId].asString(), columnType::unknown));
			}

			Term term = row.isString() ? Term(row.asString(), types)
									   : Term(row, keyValue, keyLabel, types);
			if (term.size() > 0)
			{
				add(term);

				if (row.isObject())
				{
					QMap<QString, Json::Value> controlMap;
					for (auto itr = row.begin(); itr != row.end(); ++itr)
					{
						const std::string& name = itr.key().asString();
						if (name != keyValue)
							controlMap[tq(name)] = *itr;
					}
					allControlValues[term.value()] = controlMap;
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
	return containsValue(term.value());
}

bool Terms::containsValue(const QString &value) const
{
	return _valueMap.find(value) != _valueMap.end();
}

int Terms::indexOfValue(const Term &term) const
{
	return indexOfValue(term.value());
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

			for (uint i = 0; i < _terms.size(); i++) {
				if (!v[i])
				{
					vector<string> components = _terms.at(i).scomponents();
					columnTypeVec termTypes = _terms.at(i).types();
					combination.insert(combination.end(), components.begin(), components.end());
					types.insert(types.end(), termTypes.begin(), termTypes.end());
				}
			}

			Term newTerm(combination);
			newTerm.setTypes(types);
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

			for (uint i = 0; i < _terms.size(); ++i) {
				if (!v[i])
				{
					vector<string> components = _terms.at(i).scomponents();
					columnTypeVec termTypes = _terms.at(i).types();
					combination.insert(combination.end(), components.begin(), components.end());
					types.insert(types.end(), termTypes.begin(), termTypes.end());
				}
			}

			Term newTerm(combination);
			newTerm.setTypes(types);
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

			Term newTerm(termComponents);
			newTerm.setTypes(types);
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

Json::Value Terms::getOptionsWithRelatedValues(const RelatedValuesPerTerm &relatedValuesPerTerm, const std::string &keyValue, const std::string &keyLabel, bool useArray, bool useValueAndType) const
{
	Json::Value options(Json::arrayValue);

	for (const Term& term : _terms)
	{
		QMap<QString, Json::Value> relatedValues;
		if (relatedValuesPerTerm.contains(term.value()))
			relatedValues = relatedValuesPerTerm[term.value()];

		Json::Value rowValues(Json::objectValue);

		rowValues[keyValue] = term.toJson(useArray, useValueAndType);
		if (!keyLabel.empty())
			rowValues[keyLabel] = fq(term.label());

		QMapIterator<QString, Json::Value> it2(relatedValues);
		while (it2.hasNext())
		{
			it2.next();
			rowValues[fq(it2.key())] = it2.value();
		}
		options.append(rowValues);
	}

	return options;
}

Json::Value Terms::getValuesOptions() const
{
	Json::Value options(Json::arrayValue);
	for (const Term& term : _terms)
		options.append(fq(term.value()));

	return options;
}

Json::Value Terms::getOptions(const Terms::RelatedValuesPerTerm& relatedValuesPerTerm, const std::string& keyValue, const std::string& keyLabel, bool containsInteractions, bool hasRowComponent, bool isSingleRow) const
{
	Json::Value result(Json::objectValue);

	Json::Value optionValue;

	if (hasRowComponent || containsInteractions)
		optionValue = getOptionsWithRelatedValues(relatedValuesPerTerm, keyValue, keyLabel, containsInteractions, false);
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
				if (!term.value().isEmpty() && !terms.containsValue(term))
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
