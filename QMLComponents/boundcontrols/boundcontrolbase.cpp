//
// Copyright (C) 2013-2020 University of Amsterdam
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

#include "boundcontrolbase.h"
#include "controls/jaspcontrol.h"
#include "controls/jasplistcontrol.h"
#include "analysisform.h"
#include "log.h"
#include "models/listmodel.h"
#include "controls/rowcontrols.h"

BoundControlBase::BoundControlBase(JASPControl* control) : _control(control)
{
}


//To do: define these fields (isRCode, shouldEncode, etc) somewhere centrally through an enum or something
Json::Value BoundControlBase::createMeta()  const
{ 
	Json::Value meta(Json::objectValue);
	
	if (_control->encodeValue())	
		meta["shouldEncode"] = true;
	
	for(const std::string & key : _isRCode)
		if(key.empty())		meta	 ["isRCode"] = true;
		else				meta[key]["isRCode"] = true;
	
	if(!_filterForValues.empty()) //Then the control should load a filter by this name and give it as "values" in the options it passes to the analysis
		meta["filterForValues"] = _filterForValues;

	return meta;
}

void BoundControlBase::handleComputedColumn(const Json::Value& value)
{
	if (_isColumn && value.isString())
	{
		const Json::Value & orgValue = boundValue();
		std::string			newName  = value.asString(),
							orgName  = orgValue.asString();

		if (newName != orgName)
		{
			if (!orgName.empty())
				emit _control->requestComputedColumnDestruction(orgName);
			
			if(!newName.empty())
			{
				if (_isComputedColumn)	emit _control->requestComputedColumnCreation(newName);
				else					emit _control->requestColumnCreation(newName, _columnType);
			}
			
		}
	}
}

void BoundControlBase::setBoundValue(const Json::Value &value, bool emitChange)
{
	AnalysisForm* form = _control->form();

	if (!form || !_control->isBound() || value == boundValue()) return;

	handleComputedColumn(value);
	form->setBoundValue(getName(), value, createMeta(), _control->getParentKeys());

	if (emitChange)
		emit _control->boundValueChanged(_control);
}

void BoundControlBase::setIsRCode(std::string key)
{
	_isRCode.insert(key);
}

void BoundControlBase::setFilterForValues(const std::string &filterName)
{
	_filterForValues = filterName;
}

const Json::Value & BoundControlBase::boundValue() const
{
	AnalysisForm * form = _control->form();

	if (form)	return form->boundValue(getName(), _control->getParentKeys());
	else		return Json::Value::null;
}

void BoundControlBase::setIsColumn(bool isComputed, columnType type)
{
	_isColumn			= true;
	_isComputedColumn	= isComputed;
	_columnType			= type;

	AnalysisForm * form = _control->form();
	
	if (form)	form->addColumnControl(_control, isComputed);
}


std::string BoundControlBase::getName() const
{
	return fq(_control->name());
}

void BoundControlBase::_readTableValue(const Json::Value &value, const std::string& key, bool hasMultipleTerms, Terms& terms, ListModel::RowControlsValues& allControlValues)
{
	for (const Json::Value& row : value)
	{
		std::vector<std::string> term;
		const Json::Value& keyValue = row[key];
		if (keyValue.isArray())
		{
			for (const Json::Value& component : keyValue)
				term.push_back(component.asString());
		}
		else if (keyValue.isString())
			term.push_back(keyValue.asString());
		else
			Log::log() << "Key (" << key << ") bind value is not an array or a string in " << getName() << ": " << value.toStyledString() << std::endl;

		if (term.size() > 0)
		{
			terms.add(Term(term));

			QMap<QString, Json::Value> controlMap;
			for (auto itr = row.begin(); itr != row.end(); ++itr)
			{
				const std::string& name = itr.key().asString();
				if (name != key)
					controlMap[tq(name)] = *itr;
			}

			allControlValues[Term(term).asQString()] = controlMap;
		}
	}
}

Json::Value BoundControlBase::_getTableValueOption(const Terms& terms, const ListModel::RowControlsValues& componentValuesMap, const std::string& key, bool hasInteraction, bool keyHasVariables)
{
	Json::Value result(Json::arrayValue);

	for (const Term& term : terms)
	{
		QMap<QString, Json::Value> componentValues = componentValuesMap[term.asQString()];

		Json::Value rowValues(Json::objectValue),
					keyValue; // depending of keyHasVariables & hasInteraction, keyValue can be an object, an array or a string

		if (keyHasVariables)
		{
			// If the key of the row contains variables, set 'value' and 'types'
			Json::Value jsonValue, jsonTypes;

			if (hasInteraction)
			{
				for (const std::string& comp : term.scomponents())
					jsonValue.append(comp);
				for (columnType type : term.types())
					jsonTypes.append(columnTypeToString(type));
			}
			else
			{
				jsonValue = term.asString();
				jsonTypes = columnTypeToString(term.type());
			}
			keyValue["value"] = jsonValue;
			keyValue["types"] = jsonTypes;
		}
		else
		{
			if (hasInteraction)
				for (const std::string& comp : term.scomponents())
					keyValue.append(comp);
			else
				keyValue = term.asString();
		}

		rowValues[key] = keyValue;

		QMapIterator<QString, Json::Value> it2(componentValues);
		while (it2.hasNext())
		{
			it2.next();
			rowValues[fq(it2.key())] = it2.value();
		}
		result.append(rowValues);
	}

	return result;
}

void BoundControlBase::_setTableValue(const Terms& terms, const ListModel::RowControlsValues& componentValuesMap, const std::string& key, bool hasInteraction, bool keyHasVariables)
{
	setBoundValue(_getTableValueOption(terms, componentValuesMap, key, hasInteraction, keyHasVariables));
}

bool BoundControlBase::_isValueWithTypes(const Json::Value &value) const
{
	return value.isObject() && value.isMember("types") && value.isMember("value");
}

