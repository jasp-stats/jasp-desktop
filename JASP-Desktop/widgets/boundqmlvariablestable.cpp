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

#include "boundqmlvariablestable.h"
#include "analysis/options/optionvariable.h"
#include "analysis/options/optionlist.h"
#include "analysis/options/optionboolean.h"

#include "analysis/analysisqmlform.h"
#include <QQmlProperty>
#include <QTimer>

BoundQMLVariablesTable::BoundQMLVariablesTable(QQuickItem *item, AnalysisQMLForm *form) : BoundQMLTableView(item, form)
{
	_model = _variablesTableModel = new ListModelVariablesTable(form, item);
	_variableName = _item->property("variableName").toString();
}

void BoundQMLVariablesTable::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);
	
	_tempTerms.clear();
	std::vector<Options*> optionsList = _boundTo->value();
	for (Options* options : optionsList)
	{
		OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(0));
		if (variableOption)
			_tempTerms.add(variableOption->variable());
	}
		
	_variablesTableModel->initTerms(_tempTerms);
	
	QTimer::singleShot(0, this, SLOT(_setOptions()));
}

void BoundQMLVariablesTable::unbind()
{
	
}

Option *BoundQMLVariablesTable::createOption()
{
	return new OptionsTable();
	
}

void BoundQMLVariablesTable::resetTermsFromSyncModels()
{
	_tempTerms.clear();
	for (ListModel* syncModel : _syncModels)
	{
		const Terms& terms = syncModel->terms();
		_tempTerms.add(terms);
	}
	
	_variablesTableModel->initTerms(_tempTerms);
	
	QTimer::singleShot(0, this, SLOT(_setOptions()));
}

void BoundQMLVariablesTable::_setOptions()
{
	const QMap<QString, QList<BoundQMLItem *> >& rows = _variablesTableModel->rows();
	if ((int)(_tempTerms.size()) != rows.size())
		qDebug() << "Number of terms " << _tempTerms.size() << " is not the same as the number of rows " << rows.size();
	
	std::vector<Options*> oldOptionsList = _boundTo->value();
	QMap<std::string, Options*> oldOptionsMap;
	for (Options* options : oldOptionsList)
	{
		OptionVariable* variableOption = dynamic_cast<OptionVariable*>(options->get(0));
		if (variableOption)
			oldOptionsMap[variableOption->variable()] = options;
	}
	
	std::vector<Options*> newOptionsList;
	
	for (const Term& term : _tempTerms)
	{
		QString termQStr = term.asQString();
		std::string termStr = termQStr.toStdString();
		if (!rows.contains(termQStr))
		{
			qDebug() << "Could not find " << termQStr << " in rows!!!";
			continue;
		}
		
		if (oldOptionsMap.contains(termStr))
		{
			newOptionsList.push_back(oldOptionsMap[termStr]);
		}
		else
		{
			Options* options = new Options();
			OptionVariable* optionVariable = new OptionVariable();
			optionVariable->setValue(termStr);
			options->add(_variableName.toStdString(), optionVariable);
			
			QList<BoundQMLItem* > row = rows[termQStr];
			for (BoundQMLItem* boundItem : row)
			{
				std::string name = boundItem->name().toStdString();
				Option* option = boundItem->createOption();
				options->add(name, option);				
				boundItem->bindTo(option);
			}
			newOptionsList.push_back(options);
		}
	}
	
	_boundTo->connectOptions(newOptionsList);
}
