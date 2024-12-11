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
	_optionKey = _listView->optionKey().toStdString();
}


// For interaction model, if there is no row component, the R Syntax tries to simplify the option value
// The right json value is an array of abjects of array of strings, like this:
// [
//		{
//			<optionKey> : [  value1 ]
//		}
//		{
//			<optionKey> : [ component1, component2] // In case of interaction, a value is composed by 2 components.
//		}
// ]
//
// But with R syntax, an array of strings, or an array of array of strings is allowed (the optionKey is not necessary, since no other key is used):
// [
//		value1,
//		[ component1, component2]
// ]
//
// Also if a value comes from a JASP version < 0.19, then no types value is set. So it should be added.
//
Json::Value BoundControlTerms::_adjustBindingValue(const Json::Value &value) const
{
	Json::Value adjustedValue = _isValueWithTypes(value) ? value["value"] : value;

	if (!_listView->hasRowComponent() && _listView->containsInteractions() && value.isArray())
	{
		adjustedValue = Json::Value(Json::arrayValue);
		for (const Json::Value& aValue : value)
		{
			if (aValue.isObject())
				adjustedValue.append(aValue);
			else
			{
				Json::Value row(Json::objectValue);
				Json::Value keyValue(Json::arrayValue);
				if (aValue.isString())
					keyValue.append(aValue);
				else if (aValue.isArray())
				{
					for (const Json::Value& comp : aValue)
					{
						if (comp.isString())
							keyValue.append(comp);
						else
							Log::log() << "Wrong Json type when binding " << getName() << ": " << value.toStyledString() << std::endl;
					}
				}
				else
					Log::log() << "Wrong Json type when binding " << getName() << ": " << value.toStyledString() << std::endl;

				row[_optionKey] = keyValue;
				adjustedValue.append(row);
			}
		}
	}

	return adjustedValue;
}

Json::Value BoundControlTerms::_adjustBindingType(const Json::Value &value) const
{
	Json::Value adjustedType = _isValueWithTypes(value) ? value["types"] : Json::arrayValue;
	if (adjustedType.isString())
	{
		std::string type = adjustedType.asString();
		adjustedType = Json::arrayValue;
		adjustedType.append(type);
	}

	return adjustedType;
}

void BoundControlTerms::bindTo(const Json::Value &value)
{
	Json::Value valuePart = _adjustBindingValue(value);
	Json::Value typesPart = _adjustBindingType(value);

	Terms terms;
	ListModel::RowControlsValues allControlValues;

	if (_listView->hasRowComponent() || _listView->containsInteractions())
		_readTableValue(valuePart, _optionKey, _listView->containsInteractions(), terms, allControlValues);
	else
	{
		if (valuePart.isArray())
		{
			for (const Json::Value& variable : valuePart)
				terms.add(Term(variable.asString()));
		}
		else if (valuePart.isString())
		{
			std::string str = valuePart.asString();
			if (!str.empty())
				terms.add(Term(str));
		}
		else
			Log::log() << "Control " << _control->name() << " is bound with a value that is neither an array, an object bor a string :" << valuePart.toStyledString() << std::endl;
	}

	int termId = 0;
	for (Term& term : terms)
	{
		if (typesPart.size() > termId) // If the type is given, use it
		{
			columnTypeVec types;
			if (typesPart[termId].isArray())
				for (const Json::Value& jsonType : typesPart[termId])
					types.push_back(columnTypeFromString(jsonType.asString(), columnType::unknown));
			else
				types.push_back(columnTypeFromString(typesPart[termId].asString(), columnType::unknown));
			term.setTypes(types);
		}
		termId++;
	}

	// For backward compatibility, the types of the terms must be checked.
	// Before 0.19.0, the types were not given: in this case the real type of the variable (if it is a variable) is retrieved from the dataset.
	// In 0.19.1, the types were not given for terms with interaction: in this case, the variables of the interation term is
	// most of the time also in the Variables List: the types of these variables must be stored and used for interaction term. This type might be not the one in the dataset, but a type changed by the user.
	QMap<QString, columnType> variableTypeMap;
	for (Term& term : terms)
	{
		if (term.size() == 1 && term.type() != columnType::unknown && _listView->isTypeAllowed(term.type()))
			variableTypeMap[term.asQString()] = term.type();
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

	Json::Value newValue = Json::objectValue;
	newValue["value"] = valuePart;
	newValue["types"] = terms.types();
	BoundControlBase::bindTo(newValue);

	_termsModel->initTerms(terms, allControlValues);
}

Json::Value BoundControlTerms::createJson() const
{
	return _makeOption(_termsModel->terms(), _termsModel->getTermsWithComponentValues());
}

bool BoundControlTerms::isJsonValid(const Json::Value &optionValue) const
{
	const Json::Value & valuePart = _isValueWithTypes(optionValue) ? optionValue["value"] : optionValue;
	const Json::Value & typesPart = _isValueWithTypes(optionValue) ? optionValue["types"] : Json::arrayValue;

	return	(valuePart.isNull() || valuePart.isArray() || valuePart.isString()) &&
			(typesPart.isArray() || typesPart.isString());
}

Json::Value BoundControlTerms::makeOption(const Terms& terms, const ListModel::RowControlsValues& controlValues, const std::string& optionKey, bool containsInteractions, bool hasRowComponent, bool isSingleRow)
{
	Json::Value result(Json::objectValue);

	Json::Value optionValue;

	if (hasRowComponent || containsInteractions)
		optionValue = _getTableValueOption(terms, controlValues, optionKey, containsInteractions, false);
	else if (isSingleRow)
		optionValue = terms.size() > 0 ? terms[0].asString() : "";
	else
	{
		optionValue = Json::arrayValue;
		for (const Term& term : terms)
			optionValue.append(term.asString());
	}

	result["value"] = optionValue;
	result["types"] = terms.types();

	if (hasRowComponent || containsInteractions)
		result["optionKey"] = optionKey;

	return result;
}

Json::Value BoundControlTerms::_makeOption(const Terms& terms, const ListModel::RowControlsValues& controlValues) const
{
	return makeOption(terms, controlValues, _optionKey, _listView->containsInteractions(), _listView->hasRowComponent(), _isSingleRow);
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
			newValue["optionKey"] = _optionKey;
	}

	BoundControlBase::setBoundValue(newValue.isNull() ? value : newValue, emitChanges);
}

Json::Value BoundControlTerms::addTermsToOption(const Json::Value &option, const Terms &terms, const ListModel::RowControlsValues &extraTermsMap) const
{
	Json::Value result = option;
	Terms newTerms = _getTermsFromOptions(option);
	newTerms.add(terms);

	ListModel::RowControlsValues newRowControlsValues = _termsModel->getTermsWithComponentValues();
	newRowControlsValues.insert(extraTermsMap);

	return _makeOption(newTerms, newRowControlsValues);
}

bool BoundControlTerms::areTermsInOption(const Json::Value &option, Terms &terms) const
{
	if (terms.size() == 0) return false;

	bool result = true;
	Terms termsInOptions = _getTermsFromOptions(option);
	Terms termsToSearch = terms;

	for (const Term& term : termsToSearch)
		if (termsInOptions.contains(term))	terms.remove(term);
		else								result = false;

	return result;
}

Terms BoundControlTerms::_getTermsFromOptions(const Json::Value& option) const
{
	Terms result;

	Json::Value valueOption = _isValueWithTypes(option) ? option["value"] : option;
	Json::Value typesOption = _isValueWithTypes(option) ? option["types"] : Json::nullValue;

	if (valueOption.isObject() && valueOption.isMember(_optionKey))
		valueOption = valueOption[_optionKey];

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
			if (jsonValue.isObject() && jsonValue.isMember(_optionKey))
				jsonValue = jsonValue[_optionKey];

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


