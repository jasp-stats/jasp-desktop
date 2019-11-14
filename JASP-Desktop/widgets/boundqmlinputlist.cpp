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

#include "boundqmlinputlist.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "analysis/options/optionvariable.h"
#include "log.h"
#include "listmodelextracontrols.h"

#include <QQmlProperty>
#include <QQuickItem>

BoundQMLInputList::BoundQMLInputList(QQuickItem *item, AnalysisForm *form)
	: QMLItem(item, form)
	, QMLListView(item, form)
	, BoundQMLItem()
{
	int minimumItems = getItemProperty("minimumItems").toInt();
	QString placeHolder = getItemProperty("placeHolder").toString();
	bool addVirtual = getItemProperty("addVirtual").toBool();
	_inputModel = new ListModelInputValue(this, minimumItems);
	_inputModel->setAddVirtual(addVirtual, placeHolder);
	setTermsAreNotVariables();

	_optionKeyName = getItemProperty("optionKey").toString().toStdString();
	QList<QVariant> defaultValues = getItemProperty("defaultValues").toList();
	for (QVariant defaultValue : defaultValues)
		_defaultValues.push_back(defaultValue.toString().toStdString());
	
	QQuickItem::connect(_item,	SIGNAL(itemChanged(int, QVariant)),	_inputModel,	SLOT(itemChanged(int, QVariant)));
	QQuickItem::connect(_item,	SIGNAL(itemRemoved(int)),			_inputModel,	SLOT(itemRemoved(int)));
}

void BoundQMLInputList::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable*>(option);
	if (!_boundTo)
	{
		Log::log()  << "Options for Input List " << name().toStdString() << " is not of type Table!" << std::endl;
		return;
	}
	std::vector<Options*> allOptions = _boundTo->value();

	std::vector<std::string> values;
	for (const Options* options : allOptions)
	{
		if (_hasExtraControls)
		{
			OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_optionKeyName));
			if (variableOption)
				values.push_back(variableOption->variable());
		}
		else
		{
			OptionVariables *optionVars = static_cast<OptionVariables *>(options->get(_optionKeyName));
			values = optionVars->variables();
		}
	}
	if (_hasExtraControls)
		_boundTo->setTemplateIsTemporary();
	_inputModel->initTerms(values);

}

Option* BoundQMLInputList::createOption()
{
	OptionsTable* optionsTable = nullptr;
	Options* templote = new Options();

	if (_hasExtraControls)
		templote->add(_optionKeyName, new OptionVariable());
	else
		templote->add(_optionKeyName, new OptionVariables());
	
	optionsTable = new OptionsTable(templote, _hasExtraControls);
	std::vector<Options*> allOptions;

	if (!_hasExtraControls)
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
			Options* options = new Options();
			OptionVariable* optionVar = new OptionVariable();
			optionVar->setValue(defaultValue);
			options->add(_optionKeyName, optionVar);
			allOptions.push_back(options);
		}
	}

	optionsTable->connectOptions(allOptions);
	
	return optionsTable;
}

bool BoundQMLInputList::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
}

bool BoundQMLInputList::isJsonValid(const Json::Value &optionValue)
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


// TODO: duplicate code to be removed!!!!
void BoundQMLInputList::_checkOptionTemplate()
{
	if (!_boundTo->hasTemporaryTemplate())
		return;

	const Terms& terms = model()->terms();

	if (terms.size() > 0)
	{
		const Term& firstTerm = terms.at(0);
		Options* templote = _boundTo->rowTemplate();
		ListModelExtraControls* extraControlModel = model()->getExtraControlModel(firstTerm.asQString());
		if (extraControlModel)
		{
			const QMap<QString, BoundQMLItem* >& extraControls = extraControlModel->getBoundItems();
			QMapIterator<QString, BoundQMLItem*> it(extraControls);
			while(it.hasNext())
			{
				it.next();
				std::string name = it.key().toStdString();
				Option* option = it.value()->createOption();
				templote->add(name, option);
			}
			_boundTo->setTemplate(templote);
		}
	}
}

void BoundQMLInputList::bindExtraControlOptions()
{
	if (!_hasExtraControls)
		return;

	_checkOptionTemplate();
	QMap<std::string, Options*> optionsMap;
	std::vector<Options*> allOptions = _boundTo->value();

	for (Options* options : allOptions)
	{
		OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_optionKeyName));
		if (variableOption)
			optionsMap[variableOption->variable()] = options;
		else
			Log::log()  << "An option is not of type OptionVariable!" << std::endl;
	}

	std::vector<Options*> newOptions;
	const Terms& terms = model()->terms();

	for (const Term& term : terms)
	{
		Options* rowOptions = optionsMap[term.asString()];
		if (!rowOptions)
		{
			rowOptions = static_cast<Options *>(_boundTo->rowTemplate()->clone());
			Option* option = rowOptions->get(_optionKeyName);
			OptionVariable *optionVariable = dynamic_cast<OptionVariable *>(option);
			if (optionVariable)
				optionVariable->setValue(term.asString());
			else
				Log::log() << "Option is not an OptionVariable!!!" << std::endl;
		}
		else
			rowOptions = dynamic_cast<Options*>(rowOptions->clone());

		ListModelExtraControls* extraControlModel = model()->getExtraControlModel(term.asQString());
		if (extraControlModel)
		{
			const QMap<QString, BoundQMLItem* >& extraControls = extraControlModel->getBoundItems();
			QMapIterator<QString, BoundQMLItem*> it(extraControls);
			while (it.hasNext())
			{
				it.next();
				std::string controlName = it.key().toStdString();
				BoundQMLItem* extraControl = it.value();
				Option* extraControlOption = rowOptions->get(controlName);
				if (!extraControlOption)
				{
					extraControlOption = extraControl->createOption();
					rowOptions->add(controlName, extraControlOption);
				}
				extraControl->bindTo(extraControlOption);
			}
		}
		else
			Log::log() << "Cannot find the Extra Control Model!!" << std::endl;

		newOptions.push_back(rowOptions);
	}

	_boundTo->connectOptions(newOptions);
}

void BoundQMLInputList::modelChangedHandler()
{
	if (_boundTo)
	{
		if (_hasExtraControls)
			bindExtraControlOptions();
		else
		{
			const std::vector<std::string> &values = _inputModel->terms().asVector();
			std::vector<Options *> allOptions;

			for (const std::string &value : values)
			{
				Options* options = new Options();
				options->add(_optionKeyName, new OptionString(value));
				allOptions.push_back(options);
			}
			_boundTo->setValue(allOptions);
		}
	}
	
}
