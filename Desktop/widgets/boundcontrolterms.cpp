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
#include "checkboxbase.h"
#include "analysis/options/optionvariable.h"
#include "../analysis/analysisform.h"
#include "listmodeltermsavailable.h"
#include "analysis/options/optionstable.h"
#include "analysis/options/optionterm.h"
#include "analysis/options/optionboolean.h"
#include "listmodeltermsassigned.h"
#include "listmodelinteractionassigned.h"
#include "log.h"
#include "rowcontrols.h"
#include "jasplistcontrol.h"

BoundControlTerms::BoundControlTerms(ListModelAssignedInterface* listModel, bool isSingleRow)
{
	_termsModel = listModel;
	_listView = listModel->listView();
	_isSingleRow = isSingleRow;
	_optionKey = _listView->optionKey().toStdString();
	_interactionHighOrderCheckBoxName	= _listView->property("interactionHighOrderCheckBox").toString();
}

Option* BoundControlTerms::boundTo()
{
	if (_listView->hasRowComponent() || _termsModel->areTermsInteractions())
		return _optionsTable;
	else
		return _optionVariables;
}

void BoundControlTerms::bindTo(Option *option)
{
	if (_listView->hasRowComponent() || _termsModel->areTermsInteractions())
	{
		_optionsTable = dynamic_cast<OptionsTable *>(option);
		if (!_optionsTable)
		{
			Log::log()  << "Options for list view " << _listView->name().toStdString() << " is not of type Table!" << std::endl;
			return;
		}

		if (_optionKey != _optionKeyFromFile)
		{
			// Backward compatibility: the key in the JASP file does not correspond to the current key. This must be replaced
			_optionsTable->replaceKey(_optionKeyFromFile, _optionKey);
		}
		
		std::vector<Options*> optionsList = _optionsTable->value();
		Terms terms;
		QMap<QString, QMap<QString, Option*> > allOptionsMap;
		for (Options* options : optionsList)
		{
			std::string key;
			if (_termsModel->areTermsInteractions())
			{
				OptionTerm *termOption = static_cast<OptionTerm*>(options->get(_optionKey));
				termOption->setShouldEncode(true);

				if (termOption)
				{
					Term term(termOption->term());
					key = term.asString();
					terms.add(term);
				}
				else
				{
					Log::log() << "Bind Option is not an OptionTerm in " << _listView->name().toStdString() << std::endl;
					return;
				}
			}
			else
			{
				OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_optionKey));
				if (variableOption)
				{
					key = variableOption->variable();
					terms.add(Term(key));
				}
				else
				{
					Log::log() << "Bind Option is not an OptionVariable in " << _listView->name().toStdString() << std::endl;
					return;
				}
			}
			
			QMap<QString, Option*> optionsMap;
			for (const std::string& name : options->names)
				if (name != _optionKey)
				{
					QString qname = QString::fromStdString(name);
					Option* option = options->get(name);
					if (!_interactionHighOrderCheckBoxName.isEmpty() && _interactionHighOrderCheckBoxName == qname)
						option->changed.connect(boost::bind(&BoundControlTerms::interactionHighOrderHandler, this, _1));
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
			Log::log()  << "Options for list view " << _listView->name().toStdString() << " is not of type Variables!" << std::endl;
	}
}

Option* BoundControlTerms::createOption()
{
	Option* result = nullptr;
	if (_listView->hasRowComponent() || _termsModel->areTermsInteractions())
	{
		Options* templote = new Options();

		if (_termsModel->areTermsInteractions())
		{
			OptionTerm * newOptTerm = new OptionTerm();
			newOptTerm->setShouldEncode(true);
			templote->add(_optionKey, newOptTerm);
		}
		else
			templote->add(_optionKey, new OptionVariable());

		if (_listView->hasRowComponent())
			_listView->addRowComponentsDefaultOptions(templote);
		result = new OptionsTable(templote);
	}
	else
		result = (_isSingleRow) ? new OptionVariable() : new OptionVariables();
	
	return result;
}

bool BoundControlTerms::isOptionValid(Option *option)
{
	if (_listView->hasRowComponent() || _termsModel->areTermsInteractions())	return dynamic_cast<OptionsTable*>(option) != nullptr;
	else if (_isSingleRow)														return dynamic_cast<OptionVariable*>(option) != nullptr;
	else																		return dynamic_cast<OptionVariables*>(option) != nullptr;
}

bool BoundControlTerms::isJsonValid(const Json::Value &optionValue)
{
	bool valid = true;
	if (_listView->hasRowComponent() || _termsModel->areTermsInteractions())
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
					_optionKeyFromFile = _optionKey;
					if (value[_optionKey].isNull() && value.size() > 0)
					{
						_optionKeyFromFile = value.begin().memberName();
						Log::log() << "JASP file has options for " << _listView->name() << " without '" << _optionKey << "' key. Per default, first key '" << _optionKeyFromFile << "' is used. Probably the file comes from an older version of JASP." << std::endl;
					}
					const Json::Value& components = value[_optionKeyFromFile];
					if (_termsModel->areTermsInteractions())
					{
						valid = components.type() == Json::arrayValue;
						if (components.type() == Json::stringValue)
						{
							valid = true;
							Log::log() << "JASP file has a VariableList with interaction but the elements are strings in place of arrays. Probably an old JASP file." << std::endl;
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
	else if (_isSingleRow)
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

void BoundControlTerms::updateOption()
{
	_listView->setProperty("columnsTypes", QVariant(_termsModel->itemTypes()));

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
				OptionTerm *optionTerm = dynamic_cast<OptionTerm *>(rowOptions->get(_optionKey));
				if (optionTerm)
				{
					optionTerm->setValue(term.scomponents());
					optionTerm->setShouldEncode(true);
				}
				else
					Log::log()  << "An option is not of type OptionTerm!!" << std::endl;
			}
			else
			{
				OptionVariable* optionVariable = dynamic_cast<OptionVariable*>(rowOptions->get(_optionKey));
				if (optionVariable)
					optionVariable->setValue(term.asString());
				else
					Log::log()  << "An option is not of type OptionVariable!!" << std::endl;
			}
			RowControls* rowControls = allControls[term.asQString()];
			if (rowControls)
			{
				const QMap<QString, JASPControl*>& controlsMap = rowControls->getJASPControlsMap();
				QMapIterator<QString, JASPControl*> it(controlsMap);
				while (it.hasNext())
				{
					it.next();
					BoundControl* boundItem = dynamic_cast<BoundControl*>(it.value());
					if (boundItem)
					{
						const QString& name = it.key();
						Option* option = boundItem->boundTo();
						rowOptions->add(name.toStdString(), option);
						if (!_interactionHighOrderCheckBoxName.isEmpty() && _interactionHighOrderCheckBoxName == name)
							option->changed.connect(boost::bind(&BoundControlTerms::interactionHighOrderHandler, this, _1));
					}
				}
			}
			allOptions.push_back(rowOptions);
		}

		_optionsTable->connectOptions(allOptions);
	}
	else if (_optionVariables)
		_optionVariables->setValue(_termsModel->terms().asVectorOfVectors());
}


void BoundControlTerms::interactionHighOrderHandler(Option *option)
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
		OptionTerm *termOption = static_cast<OptionTerm*>(options->get(_optionKey));
		OptionBoolean *highOrderOption = static_cast<OptionBoolean*>(options->get(_interactionHighOrderCheckBoxName.toStdString()));
		Term term = Term(termOption->term());

		if (highOrderOption->value() == checked)
		{
			for (Options* optionsBis : allOptions)
			{
				if (optionsBis == options)
					continue;

				OptionTerm *tOption = static_cast<OptionTerm*>(optionsBis->get(_optionKey));
				OptionBoolean *nOption = static_cast<OptionBoolean*>(optionsBis->get(_interactionHighOrderCheckBoxName.toStdString()));
				Term t = Term(tOption->term());

				if (checked)
				{
					if (term.containsAll(t))
					{
						if (!nOption->value())
						{
							RowControls* rowControls = _termsModel->getRowControls()[t.asQString()];
							JASPControl* control = rowControls->getJASPControlsMap()[_interactionHighOrderCheckBoxName];
							CheckBoxBase* checkBox = dynamic_cast<CheckBoxBase*>(control);
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
							JASPControl* control = rowControls->getJASPControlsMap()[_interactionHighOrderCheckBoxName];
							CheckBoxBase* checkBox = dynamic_cast<CheckBoxBase*>(control);
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

