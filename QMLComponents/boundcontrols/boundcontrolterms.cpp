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
#include "controls/variableslistbase.h"
#include "log.h"
#include "analysisform.h"
#include "models/listmodeltermsavailable.h"
#include "models/listmodeltermsassigned.h"
#include "models/listmodelinteractionassigned.h"
#include "controls/rowcontrols.h"

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

	ListModelAssignedInterface* assignedModel = qobject_cast<ListModelAssignedInterface*>(_listView->model());
	if (assignedModel && !assignedModel->checkAllowedTerms(terms))
		valuePart = addTermsToOption(Json::Value::null, terms);

	int i = 0;
	for (Term& term : terms)
	{
		if (typesPart.size() > i) // If the type is given, use it
		{
			if (typesPart[i].isArray())
			{
				columnTypeVec types;
				for (const Json::Value& jsonType : typesPart[i])
					types.push_back(columnTypeFromString(jsonType.asString(), columnType::unknown));
				term.setTypes(types);
			}
			else
				term.setType(columnTypeFromString(typesPart[i].asString(), columnType::unknown));
		}
		else
		{
			if (term.type() == columnType::unknown)
			{
				// Backward compatibility: the type was not saved before 0.19, so get the real type and check whether it is allowed. If not, take the default
				columnType type = _listView->model()->getVariableRealType(term.asQString());
				if (type != columnType::unknown && !_listView->isTypeAllowed(type))
					type = _listView->defaultType();
				term.setType(type);
			}
			typesPart.append(columnTypeToString(term.type()));
		}
		i++;
	}

	Json::Value newValue = Json::objectValue;
	newValue["value"] = valuePart;
	newValue["types"] = typesPart;
	BoundControlBase::bindTo(newValue);

	_termsModel->initTerms(terms, allControlValues);

}

Json::Value BoundControlTerms::createJson() const
{
	Json::Value valuePart,
				typesPart = Json::arrayValue;

	const Terms& terms = _termsModel->terms();

	if (_listView->containsInteractions() || _listView->hasRowComponent())
	{
		valuePart = Json::arrayValue;
		for (const Term& term : terms)
		{
			Json::Value row(Json::objectValue);
			if (_listView->containsInteractions())
			{
				Json::Value keyValue(Json::arrayValue);
				for (const std::string& comp : term.scomponents())
					keyValue.append(comp);
				row[_optionKey] = keyValue;
			}
			else
			{
				Json::Value keyValue(term.asString());
				row[_optionKey] = keyValue;
			}

			if (_listView->hasRowComponent())
			{
				auto allRowControls = _termsModel->getAllRowControls();
				if (allRowControls.contains(term.asQString()))
				{
					RowControls* rowControls = allRowControls[term.asQString()];
					const QMap<QString, JASPControl*>&	controlsMap = rowControls->getJASPControlsMap();
					for (const QString& controlName : controlsMap.keys())
					{
						JASPControl* control = controlsMap[controlName];
						BoundControl* boundControl = control->boundControl();
						if (boundControl)
							row[fq(controlName)] = boundControl->createJson();
					}
				}
			}
			valuePart.append(row);
			typesPart.append(columnTypeToString(term.type()));
		}
	}
	else if (_isSingleRow)
	{
		valuePart = (terms.size() > 0) ? terms.at(0).asString() : "";
		typesPart = columnTypeToString((terms.size() > 0) ? terms.at(0).type() : columnType::unknown);
	}
	else
	{
		valuePart = Json::arrayValue;
		for (const Term& term : terms)
		{
			valuePart.append(term.asString());
			typesPart.append(columnTypeToString(term.type()));
		}
	}

	Json::Value result = Json::objectValue;
	result["value"] = valuePart;
	result["types"] = typesPart;

	return result;
}

bool BoundControlTerms::isJsonValid(const Json::Value &optionValue) const
{
	bool valid = true;
	const Json::Value & valuePart = _isValueWithTypes(optionValue) ? optionValue["value"] : optionValue;
	const Json::Value & typesPart = _isValueWithTypes(optionValue) ? optionValue["types"] : Json::arrayValue;

	if (_listView->hasRowComponent() || _listView->containsInteractions())
	{
		valid = valuePart.type() == Json::arrayValue;
		if (valid)
		{
			for (uint i = 0; i < valuePart.size(); i++)
			{
				const Json::Value& value = valuePart[i];

				if (!_listView->hasRowComponent() && (value.type() == Json::stringValue || value.type() == Json::arrayValue))
				{
					// If there is no row component, allow stringValue (only one value) or arrayValue (for several values)
					valid = true;
				}
				else if (value.type() == Json::objectValue)
				{
					const Json::Value& components = value[_optionKey];
					valid = components.type() == Json::arrayValue || components.type() == Json::stringValue;
				}
				if (!valid)
				{
					Log::log() << "Wrong type: " << value.toStyledString() << std::endl;
					break;
				}
			}
		}
	}
	else if (_isSingleRow)
		valid = valuePart.type() == Json::stringValue;
	else
	{
		valid = valuePart.type() == Json::arrayValue;
		if (valid)
		{
			for (uint i = 0; i < valuePart.size(); i++)
			{
				const Json::Value& value = valuePart[i];
				valid = value.type() == Json::stringValue;
				if (!valid)
					break;
			}
		}
	}

	return valid && (typesPart.isArray() || typesPart.isString());
}

void BoundControlTerms::resetBoundValue()
{
	const Terms& terms = _termsModel->terms();

	if (_listView->hasRowComponent() || _listView->containsInteractions())
		_setTableValue(terms, _termsModel->getTermsWithComponentValues(), _optionKey, _listView->containsInteractions());
	else if (_isSingleRow)
	{
		std::string str = terms.size() > 0 ? terms[0].asString() : "";
		Json::Value boundValue(str);
		setBoundValue(boundValue);
	}
	else
	{
		Json::Value boundValue(Json::arrayValue);
		for (const Term& term : terms)
			boundValue.append(term.asString());
		setBoundValue(boundValue);
	}
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
			newValue["value"] = value;
			Json::Value types = _listView->valueTypes();
			if (_isSingleRow && types.isArray() && types.size() > 0)
				types = types[0];
			newValue["types"] = types;
		}
		if (_listView->hasRowComponent() || _listView->containsInteractions())
			newValue["optionKey"] = _optionKey;
	}

	BoundControlBase::setBoundValue(newValue.isNull() ? value : newValue, emitChanges);
}

Json::Value BoundControlTerms::addTermsToOption(const Json::Value &option, const Terms &terms, const ListModel::RowControlsValues &extraTermsMap) const
{
	Json::Value result = option;
	Terms termsAlreadyInOptions = _getValuesFromOptions(option);

	if (_listView->hasRowComponent() || _listView->containsInteractions())
	{
		Terms termsToAdd;
		for (const Term& term : terms)
		{
			if (!termsAlreadyInOptions.contains(term)) 
				continue; // Don't add term that is already in option.
			
			termsToAdd.add(term);
		}

		for (const Term& term : terms)
		{
			if (termsAlreadyInOptions.contains(term)) 
				continue; // Don't add term that is already in option.

			Json::Value rowValues(Json::objectValue);
			if (_listView->containsInteractions())
			{
				Json::Value keyValue(Json::arrayValue);
				for (const std::string& comp : term.scomponents())
					keyValue.append(comp);
				rowValues[_optionKey] = keyValue;
			}
			else
			{
				Json::Value keyValue(term.asString());
				rowValues[_optionKey] = keyValue;
			}

			QString termStr = term.asQString();
			if (extraTermsMap.contains(termStr))
			{
				const QMap<QString, Json::Value>& controlsMap = extraTermsMap[termStr];
				QMapIterator<QString, Json::Value> it(controlsMap);
				while (it.hasNext())
				{
					it.next();
					rowValues[fq(it.key())] = it.value();
				}
			}
			result.append(rowValues);
		}
	}
	else if (_isSingleRow)
		result = terms.size() > 0 ? terms[0].asString() : "";
	
	else
		for (const Term& term : terms)
			if (!termsAlreadyInOptions.contains(term))
				result.append(term.asString());

	return result;
}

bool BoundControlTerms::areTermsInOption(const Json::Value &option, Terms &terms) const
{
	if (terms.size() == 0) return false;

	bool result = true;
	Terms termsInOptions = _getValuesFromOptions(option);
	Terms termsToSearch = terms;

	for (const Term& term : termsToSearch)
		if (termsInOptions.contains(term))	terms.remove(term);
		else								result = false;

	return result;
}

Terms BoundControlTerms::_getValuesFromOptions(const Json::Value& _option) const
{
	Terms result;

	Json::Value option = _isValueWithTypes(option) ? _option["value"] : _option;

	if (_listView->hasRowComponent() || _listView->containsInteractions())
	{
		if (!option.isArray()) 
			return result; // Just to be sure

		for (const Json::Value& row : option)
			if (_listView->containsInteractions())
			{
				if (row.isArray())
				{
					std::vector<std::string> term;
					for (const Json::Value& val : row)
						if (val.isString())
							term.push_back(val.asString());
					
					result.add(Term(term));
				}
			}
			else if (row.isString())
				result.add(row.asString());

	}
	else if (_isSingleRow)
	{
		if (!option.isString()) 
			return result; // Just to be sure
		result.add(option.asString());
	}
	else
	{
		if (!option.isArray()) 
			return result;

		for (const Json::Value& row : option)
			if (row.isString())
				result.add(row.asString());
	}

	return result;
}


