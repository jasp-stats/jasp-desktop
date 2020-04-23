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

#include "boundqmlcomponentslist.h"
#include "analysis/jaspcontrolbase.h"
#include "rowcontrols.h"
#include "log.h"
#include "analysis/options/optionvariable.h"

BoundQMLComponentsList::BoundQMLComponentsList(JASPControlBase *item)
	: JASPControlWrapper(item)
	, QMLListView(item)
	, BoundQMLItem()
{
	_termsModel = new ListModelTermsAssigned(this);
	setTermsAreNotVariables();
	readModelProperty();

	QQuickItem::connect(_item, SIGNAL(valuesChanged()), this, SLOT(valuesChangedHandler()));
	QQuickItem::connect(_item, SIGNAL(addTerm(QString)), this, SLOT(addTermHandler(QString)));
	QQuickItem::connect(_item, SIGNAL(removeTerm(QString)), this, SLOT(removeTermHandler(QString)));
}

void BoundQMLComponentsList::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable*>(option);
	if (!_boundTo)
	{
		Log::log()  << "Options for Components List " << name().toStdString() << " is not of type Table!" << std::endl;
		return;
	}

	Terms terms;
	QMap<QString, QMap<QString, Option*> > allOptionsMap;
	std::vector<Options*> optionsList = _boundTo->value();
	for (Options* options : optionsList)
	{
		std::string key;
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

		QMap<QString, Option*> optionsMap;
		for (const std::string& name : options->names)
			if (name != _optionKeyName)
				optionsMap[QString::fromStdString(name)] = options->get(name);

		allOptionsMap[QString::fromStdString(key)] = optionsMap;

	}

	_termsModel->initTerms(terms, allOptionsMap);
}

Option* BoundQMLComponentsList::createOption()
{
	Options* templote = new Options();
	templote->add(_optionKeyName, new OptionVariable());
	addRowComponentsDefaultOptions(templote);

	OptionsTable* result = new OptionsTable(templote);
	std::vector<Options*> allOptions;

	Terms initTerms = _termsModel->getSourceTerms();
	for (const Term& term : initTerms)
	{
		Options* row = dynamic_cast<Options*>(templote->clone());
		OptionVariable* optionVar = new OptionVariable();
		optionVar->setValue(term.asString());
		row->add(_optionKeyName, optionVar);
		allOptions.push_back(row);
	}
	result->connectOptions(allOptions);

	return result;
}

bool BoundQMLComponentsList::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
}

bool BoundQMLComponentsList::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::arrayValue;
}

void BoundQMLComponentsList::modelChangedHandler()
{
	if (_boundTo)
	{
		std::vector<Options*> allOptions;
		const Terms& terms = _termsModel->terms();
		const QMap<QString, RowControls*>& allControls = _termsModel->getRowControls();
		for (const Term& term : terms)
		{
			Options *rowOptions = static_cast<Options *>(_boundTo->rowTemplate()->clone());
			OptionVariable* optionVariable = dynamic_cast<OptionVariable*>(rowOptions->get(_optionKeyName));
			if (optionVariable)
				optionVariable->setValue(term.asString());
			else
				Log::log()  << "An option is not of type OptionVariable!!" << std::endl;

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
				}
			}
			allOptions.push_back(rowOptions);
		}

		_boundTo->connectOptions(allOptions);
	}
}

void BoundQMLComponentsList::valuesChangedHandler()
{
	readModelProperty();
	modelChangedHandler();
}

void BoundQMLComponentsList::addTermHandler(QString term)
{
	const Terms& myTerms = _termsModel->terms();
	if (!myTerms.contains(term))
	{
		Terms newTerms;
		newTerms.add(term);
		_termsModel->addTerms(newTerms);
	}
}

void BoundQMLComponentsList::removeTermHandler(QString term)
{
	const std::vector<Term>& myTerms = _termsModel->terms().terms();

	int index = 0;
	for (const Term& myTerm : myTerms)
	{
		if (myTerm.asQString() == term)
			break;
		index++;
	}

	if (index < int(myTerms.size()))
	{
		QList<int> indexes = {index};
		_termsModel->removeTerms(indexes);
	}
}
