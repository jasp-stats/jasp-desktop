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
#include "extracontrolsinfo.h"

using namespace std;

BoundQMLListViewTerms::BoundQMLListViewTerms(QQuickItem* item, AnalysisForm* form, bool interaction) : QMLItem(item, form), BoundQMLListViewDraggable(item, form)
{
	_optionsTable = nullptr;
	_optionVariables = nullptr;
	_singleItem = QQmlProperty(_item, "singleVariable").read().toBool();
	bool addAvailableTermsToAssigned = _item->property("addAvailableVariablesToAssigned").toBool();
	bool mustContainLowerTerms = _item->property("mustContainLowerTerms").toBool();
	
	if (_extraControlsInfo.extraControlOptionName().empty())
		_optionKeyName = interaction ? "components" : "variable";
	else
		_optionKeyName = _extraControlsInfo.extraControlOptionName();
	
	if (interaction)
		_termsModel = new ListModelInteractionAssigned(this, addAvailableTermsToAssigned, mustContainLowerTerms);
	else
		_termsModel = new ListModelTermsAssigned(this, _singleItem);
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
				OptionTerm *termOption = static_cast<OptionTerm*>(options->get(_optionKeyName));
				if (termOption)
					terms.add(Term(termOption->term()));
			}
			else
			{
				OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_optionKeyName));
				if (variableOption)
					terms.add(variableOption->variable());
			}
		}
		
		if (_hasExtraControls)
			_optionsTable->setTemplateIsTemporary();
		// This will create the rows in QML, and in case, this will create also their extra controls
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
	Option* result = nullptr;
	if (_hasExtraControls || _termsModel->areTermsInteractions())
	{
		Options* templote = new Options();
		if (_termsModel->areTermsInteractions())
			templote->add(_optionKeyName, new OptionTerm());
		else
			templote->add(_optionKeyName, new OptionVariable());
		// If there are extra controls, the row template of the OptionsTable cannot be completely made
		// We need to wait that these extra controls are built in QML, so that we know which kind of options need the row template.
		// See _checkOptionTemplate
		result = new OptionsTable(templote, _hasExtraControls);
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
					const Json::Value& components = value[_optionKeyName];
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
	
	if (_extraControlsInfo.extraControlOptionName().empty())
		_optionKeyName = "components";
	else
		_optionKeyName = _extraControlsInfo.extraControlOptionName();
}

void BoundQMLListViewTerms::_checkOptionTemplate()
{
	if (!_optionsTable || !_optionsTable->hasTemporaryTemplate())
		return;

	// The row template might not be up to date: the createOptions does not know the extra columns.
	const Terms& terms = _termsModel->terms();

	if (terms.size() > 0)
	{
		const Term& firstTerm = terms.at(0);
		Options* templote = _optionsTable->rowTemplate();
		ListModelExtraControls* extraControlModel = _termsModel->getExtraControlModel(firstTerm.asQString());
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
			_optionsTable->setTemplate(templote);
		}
	}

}

QMap<std::string, Options*> BoundQMLListViewTerms::_transformTableOptionsInMap()
{
	QMap<std::string, Options*> optionsMap;
	std::vector<Options*> allOptions = _optionsTable->value();

	for (Options* options : allOptions)
	{
		std::string colName;
		if (_termsModel->areTermsInteractions())
		{
			OptionTerm* termOption = dynamic_cast<OptionTerm *>(options->get(_optionKeyName));
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
			OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(_optionKeyName));
			if (variableOption)
				colName = variableOption->variable();
			else
				Log::log()  << "An option is not of type OptionVariable!" << std::endl;
		}
		optionsMap[colName] = options;
	}

	return optionsMap;
}

Options* BoundQMLListViewTerms::_createRowOptions(const Term& term)
{
	Options *row = static_cast<Options *>(_optionsTable->rowTemplate()->clone());
	Option* option = row->get(_optionKeyName);
	if (_termsModel->areTermsInteractions())
	{
		OptionTerm *optionTerm = dynamic_cast<OptionTerm *>(option);
		if (optionTerm)
			optionTerm->setValue(term.scomponents());
		else
			Log::log() << "Option is not an OptionTerm!!!" << std::endl;
	}
	else
	{
		OptionVariable *optionVariable = dynamic_cast<OptionVariable *>(option);
		if (optionVariable)
			optionVariable->setValue(term.asString());
		else
			Log::log() << "Option is not an OptionVariable!!!" << std::endl;
	}

	if (_extraControlsInfo.hasNuisanceControl())
	{
		// Really special handling for nuisance extra control
		QString itemType = _termsModel->getItemType(term);
		if (itemType == "randomFactors")
		{
			OptionBoolean* option = dynamic_cast<OptionBoolean*>(row->get(_extraControlsInfo.optionNuisanceName()));
			if (option)
				option->setValue(true);
		}
	}

	return row;
}

void BoundQMLListViewTerms::bindExtraControlOptions()
{
	if (!_hasExtraControls)
		return;

	_checkOptionTemplate();
	QMap<std::string, Options*> currentTableOptionsMap = _transformTableOptionsInMap();
	vector<Options*> allOptions;
	const Terms& terms = _termsModel->terms();

	for (const Term& term : terms)
	{
		Options* rowOptions = currentTableOptionsMap[term.asString()];
		if (!rowOptions)
			rowOptions = _createRowOptions(term);
		else
			rowOptions = dynamic_cast<Options*>(rowOptions->clone());

		ListModelExtraControls* extraControlModel = _termsModel->getExtraControlModel(term.asQString());
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
				if (extraControlOption)
					extraControl->bindTo(extraControlOption);
				else
					Log::log() << "Could not find option for control " << it.key().toStdString() << std::endl;
			}
		}
		else
			Log::log() << "Cannot find the Extra Control Model!!" << std::endl;

		rowOptions->changed.connect(boost::bind(&BoundQMLListViewTerms::_extraOptionsChangedHandler, this, _1));

		allOptions.push_back(rowOptions);
	}

	_optionsTable->connectOptions(allOptions);
}

void BoundQMLListViewTerms::modelChangedHandler()
{
	if (_optionsTable)
	{
		if (_hasExtraControls)
			bindExtraControlOptions();
		else
		{
			vector<Options*> allOptions;
			const Terms& terms = _termsModel->terms();
			for (const Term& term : terms)
			{
				Options *rowOptions = static_cast<Options *>(_optionsTable->rowTemplate()->clone());
				OptionTerm *optionTerm = dynamic_cast<OptionTerm *>(rowOptions->get(_optionKeyName));
				if (optionTerm)
				{
					optionTerm->setValue(term.scomponents());
					allOptions.push_back(rowOptions);
				}
				else
					Log::log()  << "An option is not of type OptionTerm!!" << std::endl;
			}

			_optionsTable->setValue(allOptions);
		}
	}
	else if (_optionVariables)
		_optionVariables->setValue(_termsModel->terms().asVectorOfVectors());
}



void BoundQMLListViewTerms::_extraOptionsChangedHandler(Option *option)
{
	Options* options = static_cast<Options*>(option);
	if (_termsModel->areTermsInteractions() && _extraControlsInfo.hasNuisanceControl())
	{
		OptionTerm* termOption = dynamic_cast<OptionTerm *>(options->get(_optionKeyName));
		if (termOption)
		{
			OptionBoolean* nuisance = dynamic_cast<OptionBoolean*>(options->get(_extraControlsInfo.optionNuisanceName()));
			if (nuisance)
				_updateNuisances(nuisance->value());
		}
	}	
}

void BoundQMLListViewTerms::_updateNuisances(bool checked)
{
	QString nuisanceName = QString::fromStdString(_extraControlsInfo.optionNuisanceName());
	// if a higher order interaction is specified as nuisance, then all lower order terms should be changed to nuisance as well
	std::vector<Options*> allOptions = _optionsTable->value();
	for (Options* options : allOptions)
	{
		OptionTerm *termOption = static_cast<OptionTerm*>(options->get(_optionKeyName));
		OptionBoolean *nuisanceOption = static_cast<OptionBoolean*>(options->get(_extraControlsInfo.optionNuisanceName()));
		Term term = Term(termOption->term());

		if (nuisanceOption->value() == checked)
		{
			for (Options* optionsBis : allOptions)
			{
				if (optionsBis == options)
					continue;

				OptionTerm *tOption = static_cast<OptionTerm*>(optionsBis->get(_optionKeyName));
				OptionBoolean *nOption = static_cast<OptionBoolean*>(optionsBis->get(_extraControlsInfo.optionNuisanceName()));
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

