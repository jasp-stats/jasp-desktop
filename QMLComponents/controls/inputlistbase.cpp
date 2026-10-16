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

#include "inputlistbase.h"
#include "log.h"
#include "jaspcontrol.h"
#include "rowcontrols.h"


InputListBase::InputListBase(QQuickItem *parent)
	: JASPListControl(parent), BoundControlBase(this)
{
	_controlType			= ControlType::InputListView;
	_useControlMouseArea	= false;
}

void InputListBase::setUpModel()
{
	int minRows = property("minRows").toInt();
	QString placeHolder = property("placeHolder").toString();
	bool addVirtual = property("addVirtual").toBool();
	_inputModel = new ListModelInputValue(this, minRows);
	_inputModel->setAddVirtual(addVirtual, placeHolder);

	QList<QVariant> defaultValues = property("defaultValues").toList();
	for (QVariant defaultValue : defaultValues)
		_defaultValues.push_back(defaultValue.toString().toStdString());

	connect(this,	&InputListBase::itemChanged,	_inputModel,	&ListModelInputValue::itemChanged);
	connect(this,	&InputListBase::itemRemoved,	_inputModel,	&ListModelInputValue::itemRemoved);

	JASPListControl::setUpModel();
}

void InputListBase::bindTo(const Json::Value& value)
{
	BoundControlBase::bindTo(value);

	Terms::RelatedValuesPerTerm allControlValues;
	Terms terms(value, fq(_optionKeyValue), fq(_optionKeyLabel), allControlValues);

	_inputModel->initTerms(terms, allControlValues);
}

Json::Value InputListBase::createJson() const
{
	Json::Value result(Json::arrayValue);
	std::string keyName = fq(_optionKeyValue);
	
	if (_defaultValues.size() > 0)
	{
		std::vector<Options*> allOptions;

		for (const std::string& defaultValue : _defaultValues)
		{
			if (hasRowComponent())
			{
				Json::Value row(Json::objectValue);
				row[keyName] = defaultValue;
				result.append(row);
			}
			else
				result.append(defaultValue);
		}
	}

	return result;
}

bool InputListBase::isJsonValid(const Json::Value &value) const
{
	bool valid = value.isArray();

	if (valid)
	{
		for (const Json::Value& row : value)
		{
			if (hasRowComponent())	valid = row.isObject();
			else					valid = row.isString();

			if (!valid)	break;
		}
	}

	return valid;
}

void InputListBase::termsChangedHandler()
{
	JASPListControl::termsChangedHandler();

	setBoundValue(createEngineValue());
}

//mds-plan D3.1: the live input terms exactly as the write path puts them in the control's own
//value (row-component lists keep the optionKey discipline through the single
//Terms::getOptions row serialization - the D3.4 deletion folded the shuffle into it).
Json::Value InputListBase::createEngineValue() const
{
	if (!_inputModel)
		return Json::Value(Json::arrayValue);

	const Terms terms = _inputModel->terms();

	return hasRowComponent() ? terms.getOptions(_inputModel->getTermsWithComponentValues(), fq(_optionKeyValue), fq(_optionKeyLabel), containsInteractions(), true, false)["value"]
							 : terms.getValuesOptions();
}

//mds-plan D3.2: positional per-term sidecar (schema: BoundControlTerms::createFilterIds).
Json::Value InputListBase::createFilterIds() const
{
	if (!_control->encodeValue() || !_inputModel)
		return Json::nullValue;

	const int			hostId	= generatedHostFilterId();

	Json::Value ids(Json::arrayValue);
	bool		any				= false;

	for (const Term& term : _inputModel->terms())
	{
		const int id = term.filterId() >= 0 ? term.filterId() : hostId;
		any = any || id >= 0;
		ids.append(id >= 0 ? Json::Value(id) : Json::Value());
	}

	return any ? ids : Json::nullValue;
}
