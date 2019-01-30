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

#include "boundqmllistviewterms.h"
#include "analysis/options/optionvariable.h"
#include "../analysis/analysisform.h"
#include "listmodeltermsavailable.h"
#include "analysis/options/optionstable.h"
#include "listmodelextracontrols.h"

#include <QQmlProperty>
#include <QTimer>

using namespace std;

BoundQMLListViewTerms::BoundQMLListViewTerms(QQuickItem* item, AnalysisForm* form) : QMLItem(item, form), BoundQMLListViewDraggable(item, form)
{
	_optionsTable = nullptr;
	_optionVariables = nullptr;
	_singleItem = QQmlProperty(_item, "singleItem").read().toBool();
	_variablesModel = new ListModelTermsAssigned(this, _singleItem);
	
	connect(_variablesModel, &ListModelAssignedInterface::allExtraControlsLoaded, this, &BoundQMLListViewTerms::bindExtraControlOptions);
}

void BoundQMLListViewTerms::bindTo(Option *option)
{	
	if (_hasExtraControls)
	{
		_optionsTable = dynamic_cast<OptionsTable *>(option);
		if (!_optionsTable)
		{
			qDebug() << "Options for list view " << name() << " is not of type Table!";
			return;
		}
		Options* templote = new Options();
		templote->add(_extraControlVariableName, new OptionVariable());
		addExtraOptions(templote);
		_optionsTable->setTemplate(templote);
		
		Terms terms;
		std::vector<Options*> optionsList = _optionsTable->value();
		for (Options* options : optionsList)
		{
			OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(0));
			if (variableOption)
				terms.add(variableOption->variable());
		}
		
		// This will create the rows in QML, with their extra controls, and this will call the bindExtraControlOptions
		_variablesModel->initTerms(terms);
	}
	else
	{
		_optionVariables = dynamic_cast<OptionVariables *>(option);
		if (_optionVariables)
			_variablesModel->initTerms(_optionVariables->value());
		else
			qDebug() << "Options for list view " << name() << " is not of type Variables!";
	}
}

void BoundQMLListViewTerms::unbind()
{
}

Option* BoundQMLListViewTerms::createOption()
{
	Option* result;
	if (_hasExtraControls)
	{
		Options* templote = new Options();
		templote->add(_extraControlVariableName, new OptionVariable());
		addExtraOptions(templote);
		OptionsTable* optionsTable = new OptionsTable(templote);
		
		std::vector<Options*> allOptions;
		Terms terms = model()->getSourceTerms();
		for (const Term& term : terms)
		{
			Options* options = new Options();
			OptionVariable* optionVariable = new OptionVariable();
			optionVariable->setValue(term.asString());
			options->add(_extraControlVariableName, optionVariable);
			allOptions.push_back(options);
		}
		optionsTable->setValue(allOptions);
		result = optionsTable;
	}
	else
		result = _singleItem ? new OptionVariable() : new OptionVariables();
	
	return result;
}

bool BoundQMLListViewTerms::isOptionValid(Option *option)
{
	if (_hasExtraControls)
		return dynamic_cast<OptionsTable*>(option) != nullptr;
	else if (_singleItem)
		return dynamic_cast<OptionVariable*>(option) != nullptr;
	else	
		return dynamic_cast<OptionVariables*>(option) != nullptr;
}

void BoundQMLListViewTerms::modelChangedHandler()
{
	if (!_hasExtraControls)
		_optionVariables->setValue(_variablesModel->terms().asVectorOfVectors());
}

void BoundQMLListViewTerms::bindExtraControlOptions()
{
	const Terms& terms = _variablesModel->terms();
	
	std::vector<Options*> newOptionsList;
	std::vector<Options*> oldOptionsList = _optionsTable->value();
	QMap<std::string, Options*> oldOptionsMap;
	
	for (Options* options : oldOptionsList)
	{
		OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(0));
		if (!variableOption)
		{
			qDebug() << "_bindExtraControls: Could not find variable option in rows!!!";
			continue;
		}
		oldOptionsMap[variableOption->variable()] = options;
	}	
	
	for (const Term& term : terms)
	{
		QString termQStr = term.asQString();
		std::string termStr = termQStr.toStdString();
		ListModelExtraControls* extraControlModel = _variablesModel->getExtraControlModel(termQStr);
		if (!extraControlModel)
		{
			qDebug() << "connectExtraControlOptions: Could not find " << termQStr << " in rows!!!";
			continue;
		}
		
		Options* options = oldOptionsMap[termStr];
		if (!options)
		{
			options = new Options();
			OptionVariable* optionVariable = new OptionVariable();
			optionVariable->setValue(termStr);
			options->add(_extraControlVariableName, optionVariable);
		}
		
		QMap<QString, BoundQMLItem* > row = extraControlModel->getBoundItems();
		QMapIterator<QString, BoundQMLItem*> it(row);
		while (it.hasNext())
		{
			it.next();
			std::string name = it.key().toStdString();
			BoundQMLItem* boundItem = it.value();
			Option* option = options->get(name);
			if (!option)
			{
				option = boundItem->boundTo();
				if (!option)
					option = boundItem->createOption();
				options->add(name, option);
			}
			boundItem->bindTo(option);
		}
		newOptionsList.push_back(options);
	}
	
	_optionsTable->connectOptions(newOptionsList);
}
