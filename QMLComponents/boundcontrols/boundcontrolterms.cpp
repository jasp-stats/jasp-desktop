//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "boundcontrolterms.h"
#include "log.h"
#include "controls/jasplistcontrol.h"

BoundControlTerms::BoundControlTerms(ListModelAssignedInterface* listModel, bool isSingleRow) : BoundControlBase(listModel->listView())
{
	_termsModel = listModel;
	_listView = qobject_cast<JASPListControl*>(_control);
	_isSingleRow = isSingleRow;
}


void BoundControlTerms::bindTo(const Json::Value &value)
{
	Terms::RelatedValuesPerTerm allControlValues;

	//mds-plan D3.4: ONE parse step - the Terms json applier absorbs the shape normalization the
	//deleted _adjustBindingValue/_adjustBindingType helpers used to run here (and the list's
	//interaction/row-component flags decide whether the R-syntax-simplified form gets row-wrapped,
	//exactly as before). The parsed typed Terms are then copied into the model - provenance rides
	//the Term objects (mds-plan C9), and legacy unqualified JSON parses to -1 terms that the D2.1
	//wildcard lookup matches against the stamped model.
	Terms terms(value, _optionKeyValue(), _optionKeyLabel(), allControlValues,
				_listView->containsInteractions(), _listView->hasRowComponent());

	// For backward compatibility, the types of the terms must be checked.
	// Before 0.19.0, the types were not given: in this case the real type of the variable (if it is a variable) is retrieved from the dataset.
	// In 0.19.1, the types were not given for terms with interaction: in this case, the variables of the interation term is
	// most of the time also in the Variables List: the types of these variables must be stored and used for interaction term. This type might be not the one in the dataset, but a type changed by the user.
	QMap<QString, columnType> variableTypeMap;
	for (Term& term : terms)
	{
		if (term.size() == 1 && term.type() != columnType::unknown && _listView->isTypeAllowed(term.type()))
			variableTypeMap[term.value()] = term.type();
	}

	for (Term& term : terms)
	{
		columnTypeVec	types = term.types(),
						checkedTypes;
		int componentId = 0;
		for (const QString& component : term.components())
		{
			columnType type = types.size() > componentId ? types[componentId] : columnType::unknown;
			if (type == columnType::unknown)
				type = variableTypeMap.contains(component) ? variableTypeMap[component] : _listView->model()->getVariableRealType(component);

			// Ensure that the type is allowed (if it is unknown, this is not a variable, so don't check the type)
			if (type != columnType::unknown && !_listView->isTypeAllowed(type))
				type = _listView->defaultType();

			checkedTypes.push_back(type);
			componentId++;
		}

		term.setTypes(checkedTypes);
	}

	//The control's own value is the canonical serialization of what was parsed (the same builder
	//the write path and the generator use - C11), not the raw input tree the old mirror-era bind
	//stored; every consumer that reads this option reads the generated form anyway.
	BoundControlBase::bindTo(_makeOption(terms, allControlValues));

	_termsModel->initTerms(terms, allControlValues);
}

Json::Value BoundControlTerms::createJson() const
{
	return _makeOption(_termsModel->terms(), _termsModel->getTermsWithComponentValues());
}

//mds-plan C11/D3.2: the transient per-term provenance of this list, POSITIONAL over the terms
//exactly like `types` is: one entry per term, `Term::filterId()` when the term is qualified
//(>= 0, mds-plan C9), else the option's host filter (the same id createMeta stamps today - the
//C13 fallback baked in so the generated Engine json is a COMPLETE input contract for
//prepareMultiDataSetRun and the request encoding), else null (= unqualified, legacy/hostless:
//the resolver's currentEncoder fallback, exactly today's unstamped behaviour). The whole node is
//omitted when there is nothing to say, so non-aware runs generate filterId-less Engine json that
//matches today's byte-for-byte. Storage keeps the same array INSIDE the option object (D5.1
//preview): the final per-term {name, filterId?, types} reshaping happens in Phase 5; until then
//the storage shape must keep re-entering the form through today's JSON->state appliers
//(Terms parsing ignores the extra member).
Json::Value BoundControlTerms::createFilterIds() const
{
	if (!_control->encodeValue())
		return Json::nullValue;

	const int			hostId	= generatedHostFilterId();

	Json::Value ids(Json::arrayValue);
	bool		any				= false;

	for (const Term& term : _termsModel->terms())
	{
		const int id = term.filterId() >= 0 ? term.filterId() : hostId;
		any = any || id >= 0;
		ids.append(id >= 0 ? Json::Value(id) : Json::Value());
	}

	return any ? ids : Json::nullValue;
}

Json::Value BoundControlTerms::createStorageValue() const
{
	Json::Value value	= createJson();
	Json::Value	ids		= createFilterIds();

	if (!ids.isNull() && value.isObject())
		value["filterIds"] = ids;

	return value;
}

bool BoundControlTerms::isJsonValid(const Json::Value &optionValue) const
{
	const Json::Value & valuePart = _isValueWithTypes(optionValue) ? optionValue["value"] : optionValue;
	const Json::Value & typesPart = _isValueWithTypes(optionValue) ? optionValue["types"] : Json::arrayValue;

	return	(valuePart.isNull() || valuePart.isArray() || valuePart.isString()) &&
			(typesPart.isArray() || typesPart.isString());
}

Json::Value BoundControlTerms::_makeOption(const Terms& terms, const Terms::RelatedValuesPerTerm& controlValues) const
{
	return terms.getOptions(controlValues, _optionKeyValue(), _optionKeyLabel(), _listView->containsInteractions(), _listView->hasRowComponent(), _isSingleRow);
}

std::string BoundControlTerms::_optionKeyValue() const
{
	return _listView->optionKeyValue().toStdString();
}

std::string BoundControlTerms::_optionKeyLabel() const
{
	return _listView->optionKeyLabel().toStdString();
}

void BoundControlTerms::resetBoundValue()
{
	setBoundValue(_makeOption(_termsModel->terms(), _termsModel->getTermsWithComponentValues()));
}

void BoundControlTerms::setBoundValue(const Json::Value &value, bool emitChanges)
{
	Json::Value newValue;

	if (_control->encodeValue())
	{
		if (_isValueWithTypes(value))
			newValue = value;
		else
		{
			Json::Value types = _termsModel->getVariableTypes();
			if (_isSingleRow)
			{
				newValue["types"] = types.isArray() ? (types.size() > 0 ? types[0] : "") : types;
				newValue["value"] = value.isArray() ? (value.size() > 0 ? value[0] : "") : value;
			}
			else
			{
				newValue["value"] = value;
				newValue["types"] = types;
			}
		}
		if (_listView->hasRowComponent() || _listView->containsInteractions())
			newValue["optionKey"] = _optionKeyValue();
	}

	BoundControlBase::setBoundValue(newValue.isNull() ? value : newValue, emitChanges);
}

Json::Value BoundControlTerms::addTermsToOption(const Json::Value &option, const Terms &terms, const Terms::RelatedValuesPerTerm &extraTermsMap) const
{
	Json::Value result = option;
	Terms newTerms = _getTermsFromOptions(option);
	newTerms.add(terms);

	Terms::RelatedValuesPerTerm newRowControlsValues = _termsModel->getTermsWithComponentValues();
	newRowControlsValues.insert(extraTermsMap);

	return _makeOption(newTerms, newRowControlsValues);
}

bool BoundControlTerms::areTermsInOption(const Json::Value &option, Terms &terms) const
{
	int sizeBefore = terms.size();
	terms.remove(_getTermsFromOptions(option));

	return terms.size() != sizeBefore;
}

Terms BoundControlTerms::_getTermsFromOptions(const Json::Value& option) const
{
	Terms result;

	Json::Value valueOption = _isValueWithTypes(option) ? option["value"] : option;
	Json::Value typesOption = _isValueWithTypes(option) ? option["types"] : Json::nullValue;

	if (valueOption.isObject() && valueOption.isMember(_optionKeyValue()))
		valueOption = valueOption[_optionKeyValue()];

	auto parseType = [](const Json::Value& jsonType, int i = 0) -> columnType
	{
		std::string strType = (jsonType.isArray() && jsonType.size() > i ? jsonType[i].asString() : (jsonType.isString() && i == 0 ? jsonType.asString() : ""));
		return columnTypeFromString(strType, columnType::unknown);
	};

	if (valueOption.isArray())
	{
		int i = 0;
		for (Json::Value jsonValue : valueOption)
		{
			if (jsonValue.isObject() && jsonValue.isMember(_optionKeyValue()))
				jsonValue = jsonValue[_optionKeyValue()];

			const Json::Value& jsonType = typesOption.size() > i ? typesOption[i] : Json::nullValue;
			if (jsonValue.isArray())
			{
				std::vector<std::string> components;
				columnTypeVec types;
				int j = 0;
				for (const Json::Value& component : jsonValue)
				{
					components.push_back(component.asString());
					types.push_back(parseType(typesOption, j));
					j++;
				}

				result.add(Term(components, types));
			}
			else if (jsonValue.isString())
				result.add(Term(jsonValue.asString(), parseType(typesOption)));

			i++;
		}

	}
	else if (valueOption.isString())
		result.add(valueOption.asString());

	return result;
}


