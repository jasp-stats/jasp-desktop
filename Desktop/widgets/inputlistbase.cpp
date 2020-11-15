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
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "analysis/options/optionvariable.h"
#include "log.h"
#include "analysis/jaspcontrol.h"
#include "rowcontrols.h"


InputListBase::InputListBase(QQuickItem *parent)
	: JASPListControl(parent)
{
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

	QQuickItem::connect(this,	SIGNAL(itemChanged(int, QVariant)),	_inputModel,	SLOT(itemChanged(int, QVariant)));
	QQuickItem::connect(this,	SIGNAL(itemRemoved(int)),			_inputModel,	SLOT(itemRemoved(int)));
}

void InputListBase::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable*>(option);
	if (!_boundTo)
	{
		Log::log()  << "Options for Input List " << name().toStdString() << " is not of type Table!" << std::endl;
		return;
	}
	std::vector<Options*> allOptions = _boundTo->value();

	std::vector<std::string> values;
	QMap<QString, QMap<QString, Option*> > allOptionsMap;
	for (const Options* options : allOptions)
	{
		if (hasRowComponent())
		{
			OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_optionKeyName));
			if (variableOption)
			{
				QMap<QString, Option*> optionsMap;
				for (const std::string& name : options->names)
					if (name != _optionKeyName)
						optionsMap[QString::fromStdString(name)] = options->get(name);
				std::string inputValue = variableOption->variable();
				values.push_back(inputValue);
				allOptionsMap[QString::fromStdString(inputValue)] = optionsMap;
			}
			else
			{
				Log::log() << "Options for Input list " << name().toStdString() << " is not of type Variable!" << std::endl;
				return;
			}
		}
		else
		{
			OptionVariables *optionVars = static_cast<OptionVariables *>(options->get(_optionKeyName));
			values = optionVars->variables();
		}
	}
	_inputModel->initTerms(values, allOptionsMap);

}

Option* InputListBase::createOption()
{
	OptionsTable* optionsTable = nullptr;
	Options* templote = new Options();

	if (hasRowComponent())
	{
		templote->add(_optionKeyName, new OptionVariable());
		addRowComponentsDefaultOptions(templote);
	}
	else
		templote->add(_optionKeyName, new OptionVariables());
	
	optionsTable = new OptionsTable(templote);

	if (_defaultValues.size() > 0)
	{
		std::vector<Options*> allOptions;

		if (!hasRowComponent())
		{
			Options* options = new Options();
			OptionVariables* optionVars = new OptionVariables();
			optionVars->setValue(_defaultValues);
			options->add(_optionKeyName, optionVars);
			allOptions.push_back(options);
		}
		else
		{
			for (std::string defaultValue : _defaultValues)
			{
				Options* options = dynamic_cast<Options*>(templote->clone());
				OptionVariable* optionVar = new OptionVariable();
				optionVar->setValue(defaultValue);
				options->add(_optionKeyName, optionVar);
				allOptions.push_back(options);
			}
		}
		optionsTable->connectOptions(allOptions);
	}

	return optionsTable;
}

bool InputListBase::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
}

bool InputListBase::isJsonValid(const Json::Value &optionValue)
{
	bool valid = optionValue.type() == Json::arrayValue;

	if (valid)
	{
		for (uint i = 0; i < optionValue.size(); i++)
		{
			const Json::Value& value = optionValue[i];
			valid = value.type() == Json::objectValue;
			if (valid)
			{
				const Json::Value& nameOption = value[_optionKeyName];
				valid = nameOption.type() == Json::stringValue;

				if (!valid)
					break;
			}
		}
	}

	return valid;
}

void InputListBase::modelChangedHandler()
{
	if (_boundTo)
	{
		std::vector<Options *> allOptions;
		const std::vector<std::string> &values = _inputModel->terms().asVector();
		const QMap<QString, RowControls*>& allControls = _inputModel->getRowControls();
		for (const std::string &value : values)
		{
			Options* options = new Options();
			options->add(_optionKeyName, new OptionString(value));
			RowControls* rowControls = allControls[QString::fromStdString(value)];
			if (rowControls)
			{
				const QMap<QString, JASPControl*>& controlsMap = rowControls->getJASPControlsMap();
				QMapIterator<QString, JASPControl*> it(controlsMap);
				while (it.hasNext())
				{
					it.next();
					BoundControl* boundItem = dynamic_cast<BoundControl*>(it.value());
					if (boundItem)
						options->add(it.key().toStdString(), boundItem->boundTo());
				}
			}

			allOptions.push_back(options);
		}

		_boundTo->connectOptions(allOptions);
	}
}
