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
#include "analysis/jaspcontrol.h"
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

	std::string keyName = fq(_optionKey);
	Terms terms;
	ListModel::RowControlsValues allControlValues;

	if (hasRowComponent())
		_readTableValue(value, keyName, containsInteractions(), terms, allControlValues);
	else
	{
		for (const Json::Value& variable : value)
			terms.add(Term(variable.asString()));
	}

	_inputModel->initTerms(terms, allControlValues);
}

Json::Value InputListBase::createJson()
{
	Json::Value result(Json::arrayValue);
	std::string keyName = fq(_optionKey);
	
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

bool InputListBase::isJsonValid(const Json::Value &value)
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
	const Terms& terms = _inputModel->terms();
	const QMap<QString, RowControls*>& allControls = _inputModel->getAllRowControls();

	if (hasRowComponent())
		_setTableValue(terms, allControls, fq(_optionKey), containsInteractions());
	else
	{
		Json::Value boundValue(Json::arrayValue);
		for (const Term& term : terms)
			boundValue.append(term.asString());
		setBoundValue(boundValue);
	}
}
