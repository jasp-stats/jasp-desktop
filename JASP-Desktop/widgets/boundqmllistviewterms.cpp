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
#include "boundqmlcheckbox.h"
#include "analysis/options/optionvariable.h"
#include "../analysis/analysisform.h"
#include "listmodeltermsavailable.h"
#include "analysis/options/optionstable.h"
#include "analysis/options/optionterm.h"
#include "analysis/options/optionboolean.h"
#include "listmodelextracontrols.h"
#include "listmodeltermsassigned.h"
#include "listmodelinteractionassigned.h"
#include "log.h"
#include <QQmlProperty>

using namespace std;

BoundQMLListViewTerms::BoundQMLListViewTerms(QQuickItem* item, AnalysisForm* form, bool interaction) : QMLItem(item, form), BoundQMLListViewDraggable(item, form)
{
	_optionsTable = nullptr;
	_optionVariables = nullptr;
	_singleItem = QQmlProperty(_item, "singleVariable").read().toBool();
	QString extraControlOptionName = _item->property("extraControlOptionName").toString();
	bool addAvailableTermsToAssigned = _item->property("addAvailableVariablesToAssigned").toBool();
	
	if (extraControlOptionName.isEmpty())
		_extraControlOptionName = interaction ? "components" : "variable";
	else
		_extraControlOptionName = extraControlOptionName.toStdString();
	
	if (interaction)
		_termsModel = new ListModelInteractionAssigned(this, addAvailableTermsToAssigned);
	else
		_termsModel = new ListModelTermsAssigned(this, _singleItem);

	connect(_termsModel, &ListModelAssignedInterface::extraControlsChanged, this, &BoundQMLListViewTerms::bindExtraControlOptions);
}

void BoundQMLListViewTerms::bindTo(Option *option)
{	
	if (_hasExtraControls || _termsModel->areTermsInteractions())
	{
		_optionsTable = dynamic_cast<OptionsTable *>(option);
		if (!_optionsTable)
		{
			Log::log()  << "Options for list view " << name().toStdString() << " is not of type Table!" << std::endl;
			return;
		}
		
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
		// That will also call bindExtraControlOptions
		_termsModel->initTerms(terms);
	}
	else
	{
		_optionVariables = dynamic_cast<OptionVariables *>(option);
		if (_optionVariables)
			_termsModel->initTerms(_optionVariables->value());
		else
			Log::log()  << "Options for list view " << name().toStdString() << " is not of type Variables!" << std::endl;
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
			templote->add(_extraControlOptionName, new OptionTerm());
		else
			templote->add(_extraControlOptionName, new OptionVariable());
		if (_hasExtraControls)
			addExtraOptions(templote);
		result = new OptionsTable(templote);		
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

bool BoundQMLListViewTerms::isJsonValid(const Json::Value &optionValue)
{
	bool valid = true;
	if (_hasExtraControls || _termsModel->areTermsInteractions())
	{
		valid = optionValue.type() == Json::arrayValue;
		if (valid)
		{
			for (uint i = 0; i < optionValue.size(); i++)
			{
				const Json::Value& value = optionValue[i];
				valid = value.type() == Json::objectValue;
				if (valid)
				{
					const Json::Value& components = value[_extraControlOptionName];
					if (_termsModel->areTermsInteractions())
						valid = components.type() == Json::arrayValue;
					else
						valid = components.type() == Json::stringValue;
				}
				if (!valid)
					break;
			}
		}
	}
	else if (_singleItem)
		valid = optionValue.type() == Json::stringValue;
	else
	{
		valid = optionValue.type() == Json::arrayValue;
		if (valid)
		{
			for (uint i = 0; i < optionValue.size(); i++)
			{
				const Json::Value& value = optionValue[i];
				valid = value.type() == Json::stringValue;
				if (!valid)
					break;
			}
		}
	}

	return valid;
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
					Log::log()  << "An option is not of type OptionTerm!!" << std::endl;
			}
			
			_optionsTable->setValue(allOptions);
		}
		else if (_optionVariables)
			_optionVariables->setValue(_termsModel->terms().asVectorOfVectors());
	}
}

void BoundQMLListViewTerms::_fillOptionsMap(QMap<std::string, Options*>& optionsMap)
{
	std::vector<Options*> oldOptionsList = _optionsTable->value();

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
				Log::log()  << "An option is not of type OptionTerm!" << std::endl;
		}
		else
		{
			OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_extraControlOptionName));
			if (variableOption)
				colName = variableOption->variable();
			else
				Log::log()  << "An option os not of type OptionVariable!" << std::endl;
		}
		optionsMap[colName] = options;
	}
}

void BoundQMLListViewTerms::bindExtraControlOptions()
{
	if (!_optionsTable)
		return;
	
	const Terms& terms = _termsModel->terms();
	
	std::vector<Options*> newOptionsList;
	QMap<std::string, Options*> oldOptionsMap;
	_fillOptionsMap(oldOptionsMap);
	
	// For each term, check wether an option exists already, if not create a new one
	// Then bind this option to the appropriate BoundQMLItem object
	for (const Term& term : terms)
	{
		QString termQStr = term.asQString();
		std::string termStr = termQStr.toStdString();
		ListModelExtraControls* extraControlModel = _termsModel->getExtraControlModel(termQStr);
		if (!extraControlModel)
		{
			Log::log() << "connectExtraControlOptions: Could not find " << termStr << " in rows!" << std::endl;
			continue;
		}
		
		Options* options = oldOptionsMap[termStr];
		if (!options)
		{
			options = static_cast<Options *>(_optionsTable->rowTemplate()->clone());
			initExtraControlOptions(termQStr, options);
			if (_termsModel->areTermsInteractions())
			{
				OptionTerm* optionTerm = dynamic_cast<OptionTerm *>(options->get(_extraControlOptionName));
				if (optionTerm)
					optionTerm->setValue(term.scomponents());
				else
					Log::log() << "An option is not of type OptionTerm!!!!" << std::flush;
			}
			else
			{
				OptionVariable* optionVariable = dynamic_cast<OptionVariable *>(options->get(_extraControlOptionName));
				if (optionVariable)
					optionVariable->setValue(termStr);
				else
					Log::log() << "An option is not of type OptionVariable!!!!" << std::flush;
			}
			options->changed.connect(boost::bind(&BoundQMLListViewTerms::extraOptionsChangedSlot, this, _1));
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
				// The rowTemplate should have given all options
				// But never know...
				Log::log() << "It should never come here!!!" << std::flush;
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

void BoundQMLListViewTerms::extraOptionsChangedSlot(Option *option)
{
	Options* options = dynamic_cast<Options*>(option);
	
	if (_termsModel->areTermsInteractions() && _hasNuisanceControl)
	{
		OptionTerm* termOption = dynamic_cast<OptionTerm *>(options->get(_extraControlOptionName));
		if (termOption)
		{
			OptionBoolean* nuisance = dynamic_cast<OptionBoolean*>(options->get(_nuisanceName));
			if (nuisance)
				updateNuisances(nuisance->value());
		}
	}	
}

void BoundQMLListViewTerms::initExtraControlOptions(const QString &colName, Options *options)
{
	if (_hasNuisanceControl)
	{
		QString itemType = _termsModel->getItemType(colName);
		if (itemType == "randomFactors")
		{
			OptionBoolean* option = dynamic_cast<OptionBoolean*>(options->get(_nuisanceName));
			if (option)
				option->setValue(true);
		}
	}
}

void BoundQMLListViewTerms::updateNuisances(bool checked)
{
	QString nuisanceName = QString::fromStdString(_nuisanceName);
	// if a higher order interaction is specified as nuisance, then all lower order terms should be changed to nuisance as well
	std::vector<Options*> allOptions = _optionsTable->value();
	for (Options* options : allOptions)
	{
		OptionTerm *termOption = static_cast<OptionTerm*>(options->get(_extraControlOptionName));
		OptionBoolean *nuisanceOption = static_cast<OptionBoolean*>(options->get(_nuisanceName));
		Term term = Term(termOption->term());

		if (nuisanceOption->value() == checked)
		{
			for (Options* optionsBis : allOptions)
			{
				if (optionsBis == options)
					continue;

				OptionTerm *tOption = static_cast<OptionTerm*>(optionsBis->get(_extraControlOptionName));
				OptionBoolean *nOption = static_cast<OptionBoolean*>(optionsBis->get(_nuisanceName));
				Term t = Term(tOption->term());

				if (checked)
				{
					if (term.containsAll(t))
					{
						if (!nOption->value())
						{
							ListModelExtraControls* extraControlModel = _termsModel->getExtraControlModel(t.asQString());
							const QMap<QString, BoundQMLItem* >& QMLItemMap = extraControlModel->getBoundItems();
							BoundQMLCheckBox* checkBox = dynamic_cast<BoundQMLCheckBox*>(QMLItemMap[nuisanceName]);
							if (checkBox)
								checkBox->setQMLItemChecked(true);							
							nOption->setValue(true);
						}
					}
				}
				else
				{
					if (t.containsAll(term))
					{
						if (nOption->value())
						{
							ListModelExtraControls* extraControlModel = _termsModel->getExtraControlModel(t.asQString());
							const QMap<QString, BoundQMLItem* >& QMLItemMap = extraControlModel->getBoundItems();
							BoundQMLCheckBox* checkBox = dynamic_cast<BoundQMLCheckBox*>(QMLItemMap[nuisanceName]);
							if (checkBox)
								checkBox->setQMLItemChecked(false);							
							
							nOption->setValue(false);
						}
					}
				}
			}
		}
	}
}

