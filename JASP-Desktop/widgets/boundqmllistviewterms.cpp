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
#include "listmodeltermsassigned.h"
#include "listmodelinteractionassigned.h"
#include "listmodelmultitermsassigned.h"
#include "log.h"
#include "rowcontrols.h"

BoundQMLListViewTerms::BoundQMLListViewTerms(JASPControlBase* item, bool interaction)
	: JASPControlWrapper(item)
	, BoundQMLListViewDraggable(item)
{
	_optionsTable = nullptr;
	_optionVariables = nullptr;
	_maxRows = getItemProperty("maxRows").toInt();
	_columns = getItemProperty("columns").toInt();
	bool interactionContainLowerTerms	= getItemProperty("interactionContainLowerTerms").toBool();
	_interactionHighOrderCheckBoxName	= getItemProperty("interactionHighOrderCheckBox").toString();
	bool addInteractionsByDefault		= getItemProperty("addInteractionsByDefault").toBool();
		
	if (interaction)
		_termsModel = new ListModelInteractionAssigned(this, interactionContainLowerTerms, addInteractionsByDefault);
	else if (_columns > 1)
		_termsModel = new ListModelMultiTermsAssigned(this, _columns);
	else
		_termsModel = new ListModelTermsAssigned(this, _maxRows);
}

void BoundQMLListViewTerms::bindTo(Option *option)
{
	if (_columns > 1)
	{
		_optionVariablesGroups = dynamic_cast<OptionVariablesGroups *>(option);
		_termsModel->initTerms(_optionVariablesGroups->value());
	}
	else if (_hasRowComponents || _termsModel->areTermsInteractions())
	{
		_optionsTable = dynamic_cast<OptionsTable *>(option);
		if (!_optionsTable)
		{
			Log::log()  << "Options for list view " << name().toStdString() << " is not of type Table!" << std::endl;
			return;
		}

		if (!_tempOptionKey.empty() && _tempOptionKey != _optionKeyName)
		{
			// Backward compatibility: the key in the JASP file does not correspond to the current key. This must be replacec
			_optionsTable->replaceKey(_tempOptionKey, _optionKeyName);
		}
		
		Terms terms;
		QMap<QString, QMap<QString, Option*> > allOptionsMap;
		std::vector<Options*> optionsList = _optionsTable->value();
		for (Options* options : optionsList)
		{
			std::string key;
			if (_termsModel->areTermsInteractions())
			{
				OptionTerm *termOption = static_cast<OptionTerm*>(options->get(_optionKeyName));
				if (termOption)
				{
					Term term(termOption->term());
					key = term.asString();
					terms.add(term);
				}
				else
				{
					Log::log() << "Bind Option is not an OptionTerm in " << name().toStdString() << std::endl;
					return;
				}
			}
			else
			{
				OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_optionKeyName));
				if (variableOption)
				{
					key = variableOption->variable();
					terms.add(Term(key));
				}
				else
				{
					Log::log() << "Bind Option is not an OptionVariable in " << name().toStdString() << std::endl;
					return;
				}
			}
			QMap<QString, Option*> optionsMap;
			for (const std::string& name : options->names)
				if (name != _optionKeyName)
				{
					QString qname = QString::fromStdString(name);
					Option* option = options->get(name);
					if (!_interactionHighOrderCheckBoxName.isEmpty() && _interactionHighOrderCheckBoxName == qname)
						option->changed.connect(boost::bind(&BoundQMLListViewTerms::interactionHighOrderHandler, this, _1));
					optionsMap[qname] = option;
				}
			allOptionsMap[QString::fromStdString(key)] = optionsMap;
		}
		
		_termsModel->initTerms(terms, allOptionsMap);
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

Option* BoundQMLListViewTerms::createOption()
{
	Option* result = nullptr;
	if (_columns > 1)
		result = new OptionVariablesGroups();
	else if (_hasRowComponents || _termsModel->areTermsInteractions())
	{
		Options* templote = new Options();
		if (_tempOptionKey.empty())
			_tempOptionKey = _optionKeyName;

		if (_termsModel->areTermsInteractions())
			templote->add(_tempOptionKey, new OptionTerm());
		else
			templote->add(_tempOptionKey, new OptionVariable());
		if (_hasRowComponents)
			addRowComponentsDefaultOptions(templote);
		result = new OptionsTable(templote);
	}
	else
		result = (_maxRows == 1) ? new OptionVariable() : new OptionVariables();
	
	return result;
}

bool BoundQMLListViewTerms::isOptionValid(Option *option)
{
	if (_columns > 1)
		return dynamic_cast<OptionVariablesGroups*>(option) != nullptr;
	else if (_hasRowComponents || _termsModel->areTermsInteractions())
		return dynamic_cast<OptionsTable*>(option) != nullptr;
	else if (_maxRows == 1)
		return dynamic_cast<OptionVariable*>(option) != nullptr;
	else	
		return dynamic_cast<OptionVariables*>(option) != nullptr;
}

bool BoundQMLListViewTerms::isJsonValid(const Json::Value &optionValue)
{
	bool valid = true;
	if (_columns > 1)
		valid = optionValue.type() == Json::arrayValue;
	else if (_hasRowComponents || _termsModel->areTermsInteractions())
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
					_tempOptionKey = _optionKeyName;
					if (value[_tempOptionKey].isNull() && value.size() > 0)
					{
						_tempOptionKey = value.begin().memberName();
						Log::log() << "JASP file has options for " << name() << " without '" << _optionKeyName << "' key. Per default, first key '" << _tempOptionKey << "' is used. Probably the file comes from an older version of JASP." << std::endl;
					}
					const Json::Value& components = value[_tempOptionKey];
					if (_termsModel->areTermsInteractions())
					{
						valid = components.type() == Json::arrayValue;
						if (!valid)
						{
							valid = (components.type() == Json::stringValue);
							if (valid)
								Log::log() << "JASP file has a VariableList with interaction but the elements are strings in place of arrays. Probably an old JASP file" << std::endl;
						}
					}
					else
						valid = components.type() == Json::stringValue;
				}
				if (!valid)
					break;
			}
		}
	}
	else if (_maxRows == 1)
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

void BoundQMLListViewTerms::modelChangedHandler()
{
	setItemProperty("columnsTypes", QVariant(_termsModel->itemTypes()));

	if (_optionsTable)
	{
		std::vector<Options*> allOptions;
		const Terms& terms = _termsModel->terms();
		const QMap<QString, RowControls*>& allControls = _termsModel->getRowControls();
		for (const Term& term : terms)
		{
			Options *rowOptions = static_cast<Options *>(_optionsTable->rowTemplate()->clone());
			if (_termsModel->areTermsInteractions())
			{
				OptionTerm *optionTerm = dynamic_cast<OptionTerm *>(rowOptions->get(_optionKeyName));
				if (optionTerm)
					optionTerm->setValue(term.scomponents());
				else
					Log::log()  << "An option is not of type OptionTerm!!" << std::endl;
			}
			else
			{
				OptionVariable* optionVariable = dynamic_cast<OptionVariable*>(rowOptions->get(_optionKeyName));
				if (optionVariable)
					optionVariable->setValue(term.asString());
				else
					Log::log()  << "An option is not of type OptionVariable!!" << std::endl;
			}
			RowControls* rowControls = allControls[term.asQString()];
			if (rowControls)
			{
				const QMap<QString, JASPControlWrapper*>& controlsMap = rowControls->getJASPControlsMap();
				QMapIterator<QString, JASPControlWrapper*> it(controlsMap);
				while (it.hasNext())
				{
					it.next();
					BoundQMLItem* boundItem = dynamic_cast<BoundQMLItem*>(it.value());
					const QString& name = it.key();
					Option* option = boundItem->boundTo();
					rowOptions->add(name.toStdString(), option);
					if (!_interactionHighOrderCheckBoxName.isEmpty() && _interactionHighOrderCheckBoxName == name)
						option->changed.connect(boost::bind(&BoundQMLListViewTerms::interactionHighOrderHandler, this, _1));

				}
			}
			allOptions.push_back(rowOptions);
		}

		_optionsTable->connectOptions(allOptions);
	}
	else if (_optionVariables)
		_optionVariables->setValue(_termsModel->terms().asVectorOfVectors());
	else if (_optionVariablesGroups)
	{
		ListModelMultiTermsAssigned* multiTermsModel = dynamic_cast<ListModelMultiTermsAssigned*>(_termsModel);
		if (multiTermsModel)
		{
			const QList<Terms>& tuples = multiTermsModel->tuples();
			std::vector<std::vector<std::string> > values;
			for (const Terms& terms : tuples)
				values.push_back(terms.asVector());
			_optionVariablesGroups->setValue(values);
		}
	}
}


void BoundQMLListViewTerms::interactionHighOrderHandler(Option *option)
{
	OptionBoolean* optionChanged = dynamic_cast<OptionBoolean*>(option);
	if (!optionChanged)
	{
		Log::log() << "High order option is not of type OptionBoolean!!" << std::endl;
		return;
	}

	bool checked = optionChanged->value();
	_optionsTable->blockSignals(true);
	// if a higher order interaction is specified as nuisance, then all lower order terms should be changed to nuisance as well
	std::vector<Options*> allOptions = _optionsTable->value();
	for (Options* options : allOptions)
	{
		OptionTerm *termOption = static_cast<OptionTerm*>(options->get(_optionKeyName));
		OptionBoolean *highOrderOption = static_cast<OptionBoolean*>(options->get(_interactionHighOrderCheckBoxName.toStdString()));
		Term term = Term(termOption->term());

		if (highOrderOption->value() == checked)
		{
			for (Options* optionsBis : allOptions)
			{
				if (optionsBis == options)
					continue;

				OptionTerm *tOption = static_cast<OptionTerm*>(optionsBis->get(_optionKeyName));
				OptionBoolean *nOption = static_cast<OptionBoolean*>(optionsBis->get(_interactionHighOrderCheckBoxName.toStdString()));
				Term t = Term(tOption->term());

				if (checked)
				{
					if (term.containsAll(t))
					{
						if (!nOption->value())
						{
							RowControls* rowControls = _termsModel->getRowControls()[t.asQString()];
							JASPControlWrapper* control = rowControls->getJASPControlsMap()[_interactionHighOrderCheckBoxName];
							BoundQMLCheckBox* checkBox = dynamic_cast<BoundQMLCheckBox*>(control);
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
							RowControls* rowControls = _termsModel->getRowControls()[t.asQString()];
							JASPControlWrapper* control = rowControls->getJASPControlsMap()[_interactionHighOrderCheckBoxName];
							BoundQMLCheckBox* checkBox = dynamic_cast<BoundQMLCheckBox*>(control);
							if (checkBox)
								checkBox->setQMLItemChecked(false);							
							
							nOption->setValue(false);
						}
					}
				}
			}
		}
	}
	_optionsTable->blockSignals(false, false);
}

