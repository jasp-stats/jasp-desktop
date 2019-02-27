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
#include "analysis/options/optionterm.h"
#include "listmodelextracontrols.h"
#include "listmodeltermsassigned.h"
#include "listmodelinteractionassigned.h"

#include <QQmlProperty>

using namespace std;

BoundQMLListViewTerms::BoundQMLListViewTerms(QQuickItem* item, AnalysisForm* form, bool interaction) : QMLItem(item, form), BoundQMLListViewDraggable(item, form)
{
	_optionsTable = nullptr;
	_optionVariables = nullptr;
	_singleItem = QQmlProperty(_item, "singleVariable").read().toBool();
	QString extraControlOptionName = QQmlProperty(_item, "extraControlOptionName").read().toString();
	
	if (extraControlOptionName.isEmpty())
		_extraControlOptionName = interaction ? "components" : "variable";
	else
		_extraControlOptionName = extraControlOptionName.toStdString();
	
	if (interaction)
		_termsModel = new ListModelInteractionAssigned(this);
	else
		_termsModel = new ListModelTermsAssigned(this, _singleItem);
	
	connect(_termsModel, &ListModelAssignedInterface::allExtraControlsLoaded, this, &BoundQMLListViewTerms::bindExtraControlOptions);
}

void BoundQMLListViewTerms::bindTo(Option *option)
{	
	if (_hasExtraControls || _termsModel->areTermsInteractions())
	{
		_optionsTable = dynamic_cast<OptionsTable *>(option);
		if (!_optionsTable)
		{
			qDebug() << "Options for list view " << name() << " is not of type Table!";
			return;
		}
		Options* templote = new Options();
		if (_termsModel->areTermsInteractions())
			templote->add(_extraControlOptionName, new OptionTerm());
		else
			templote->add(_extraControlOptionName, new OptionVariable());
		if (_hasExtraControls)
			addExtraOptions(templote);
		_optionsTable->setTemplate(templote);
		
		Terms terms;
		std::vector<Options*> optionsList = _optionsTable->value();
		for (Options* options : optionsList)
		{
			if (_termsModel->areTermsInteractions())
			{
				OptionTerm *termOption = static_cast<OptionTerm*>(options->get(_extraControlOptionName));
				if (termOption)
					terms.add(Term(termOption->term()));
			}
			else
			{
				OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_extraControlOptionName));
				if (variableOption)
					terms.add(variableOption->variable());
			}
		}
		
		// This will create the rows in QML, and if needed, this will create also their extra controls
		// In that case, when all rows are loaded in QML, bindExtraControlOptions will be called.
		_termsModel->initTerms(terms);
	}
	else
	{
		_optionVariables = dynamic_cast<OptionVariables *>(option);
		if (_optionVariables)
			_termsModel->initTerms(_optionVariables->value());
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
	if (_hasExtraControls || _termsModel->areTermsInteractions())
	{
		Options* templote = new Options();
		if (_termsModel->areTermsInteractions())
			templote->add(_extraControlComponentName, new OptionTerm());
		else
			templote->add(_extraControlOptionName, new OptionVariable());
		if (_hasExtraControls)
			addExtraOptions(templote);
		OptionsTable* optionsTable = new OptionsTable(templote);
		
		std::vector<Options*> allOptions;
		const Terms& terms = _termsModel->terms();
		for (const Term& term : terms)
		{
			Options* options = static_cast<Options *>(templote->clone());
			if (_termsModel->areTermsInteractions())
			{
				OptionTerm* optionTerm = dynamic_cast<OptionTerm *>(options->get(_extraControlComponentName));
				if (optionTerm)
					optionTerm->setValue(term.scomponents());
				else
					qDebug() << "An option is not of type OptionTerm!";
			}
			else
			{
				OptionVariable* optionVariable = dynamic_cast<OptionVariable *>(options->get(_extraControlOptionName));
				if (optionVariable)
					optionVariable->setValue(term.asString());
				else
					qDebug() << "An option is not of type OptionVariable!";
			}
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
	if (_hasExtraControls || _termsModel->areTermsInteractions())
		return dynamic_cast<OptionsTable*>(option) != nullptr;
	else if (_singleItem)
		return dynamic_cast<OptionVariable*>(option) != nullptr;
	else	
		return dynamic_cast<OptionVariables*>(option) != nullptr;
}

void BoundQMLListViewTerms::setTermsAreInteractions()
{
	BoundQMLListViewDraggable::setTermsAreInteractions();
	QString extraControlOptionName = QQmlProperty(_item, "extraControlOptionName").read().toString();
	
	if (extraControlOptionName.isEmpty())
		_extraControlOptionName = "components";
}

void BoundQMLListViewTerms::modelChangedHandler()
{
	if (!_hasExtraControls)
	{
		if (_termsModel->areTermsInteractions() && _optionsTable)
		{
			vector<Options*> allOptions;
			const Terms& terms = _termsModel->terms();
			for (const Term& term : terms)
			{
				Options *row = static_cast<Options *>(_optionsTable->rowTemplate()->clone());
				OptionTerm *optionTerm = dynamic_cast<OptionTerm *>(row->get(_extraControlOptionName));
				if (optionTerm)
				{
					optionTerm->setValue(term.scomponents());
					allOptions.push_back(row);
				}
				else
					qDebug() << "An option is not of type OptionTerm!!";
			}
			
			_optionsTable->setValue(allOptions);
		}
		else if (_optionVariables)
			_optionVariables->setValue(_termsModel->terms().asVectorOfVectors());
	}
}

void BoundQMLListViewTerms::bindExtraControlOptions()
{
	const Terms& terms = _termsModel->terms();
	
	std::vector<Options*> newOptionsList;
	std::vector<Options*> oldOptionsList = _optionsTable->value();
	QMap<std::string, Options*> oldOptionsMap;
	
	for (Options* options : oldOptionsList)
	{
		std::string colName;
		if (_termsModel->areTermsInteractions())
		{
			OptionTerm* termOption = dynamic_cast<OptionTerm *>(options->get(_extraControlOptionName));
			if (termOption)
			{
				Term tempTerm(termOption->term());
				colName = tempTerm.asString();
			}
			else
				qDebug() << "An option is not of type OptionTerm!!!";
		}
		else
		{
			OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_extraControlOptionName));
			if (variableOption)
				colName = variableOption->variable();
			else
				qDebug() << "An option os not of type OptionVariable!!!";
		}
		oldOptionsMap[colName] = options;
	}	
	
	for (const Term& term : terms)
	{
		QString termQStr = term.asQString();
		std::string termStr = termQStr.toStdString();
		ListModelExtraControls* extraControlModel = _termsModel->getExtraControlModel(termQStr);
		if (!extraControlModel)
		{
			qDebug() << "connectExtraControlOptions: Could not find " << termQStr << " in rows!!!";
			continue;
		}
		
		Options* options = oldOptionsMap[termStr];
		if (!options)
		{
			options = static_cast<Options *>(_optionsTable->rowTemplate()->clone());
			_termsModel->initExtraControlOptions(termQStr, options);
			if (_termsModel->areTermsInteractions())
			{
				OptionTerm* optionTerm = dynamic_cast<OptionTerm *>(options->get(_extraControlOptionName));
				if (optionTerm)
					optionTerm->setValue(term.scomponents());
				else
					qDebug() << "An option is not of type OptionTerm!!!!";
			}
			else
			{
				OptionVariable* optionVariable = dynamic_cast<OptionVariable *>(options->get(_extraControlOptionName));
				if (optionVariable)
					optionVariable->setValue(termStr);
				else
					qDebug() << "An option is not of type OptionVariable!!!!";
			}
		}
		
		const QMap<QString, BoundQMLItem* >& row = extraControlModel->getBoundItems();
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
