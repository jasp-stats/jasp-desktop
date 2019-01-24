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

#include "boundqmllistviewinteraction.h"
#include "../analysis/analysisform.h"
#include "analysis/options/optionboolean.h"
#include "listmodeltermsavailable.h"
#include "listmodelextracontrols.h"
#include <QQmlProperty>

using namespace std;

BoundQMLListViewInteraction::BoundQMLListViewInteraction(QQuickItem* item, AnalysisForm* form) 
	: QMLItem(item, form)
	, BoundQMLListViewDraggable(item, form)
{
	_boundTo = nullptr;
	_interactionModel = new ListModelInteractionAssigned(this);
	setTermsAreNotVariables();	
}

void BoundQMLListViewInteraction::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);
	Options* options = new Options();
	options->add("components", new OptionTerm());
	if (_hasExtraControls)
		addExtraOptions(options);
	_boundTo->setTemplate(options);
	
	_interactionModel->initTermsWithTemplate(_boundTo->value(), _boundTo->rowTemplate());
}

void BoundQMLListViewInteraction::unbind()
{
	
}

Option* BoundQMLListViewInteraction::createOption()
{
	Options* templote = new Options();
	templote->add("components", new OptionTerm());
	if (_hasExtraControls)
		addExtraOptions(templote);
	
	OptionsTable* result = new OptionsTable(templote);
	
	std::vector<Options *> values;
	const Terms& availableTerms = _sourceModel->terms();
	for (auto availableTerm : availableTerms)
	{
		Options *row = static_cast<Options *>(templote->clone());
		OptionTerms *termCell = static_cast<OptionTerms *>(row->get(0));
		termCell->setValue(availableTerm.scomponents());
		values.push_back(row);
	}
	result->setValue(values);
	
	return result;
}

void BoundQMLListViewInteraction::modelChangedHandler()
{
	qDebug("Model Changed handler");
	const std::vector<Options *>& rows = _interactionModel->rows();
	
	if (_hasExtraControls)
	{
		for (Options* row : rows)
		{
			OptionTerms *termOption = static_cast<OptionTerms *>(row->get(0));
			Term t(termOption->value().front());
			QString termStr = t.asQString();
			ListModelExtraControls* extraControlModel = _interactionModel->getExtraControlModel(termStr);
			
			const QMap<QString, BoundQMLItem*>& controlMap = extraControlModel->getBoundItems();
			QMapIterator<QString, BoundQMLItem*> it(controlMap);
			while (it.hasNext())
			{
				it.next();
				QString optionName = it.key();
				BoundQMLItem* boundItem = it.value();
				Option* option = row->get(optionName.toStdString());
				if (option)
					boundItem->bindTo(option);
				else
					qDebug() << "No option found with name " << optionName;
			}
		}
	}
	
	_boundTo->connectOptions(rows);
}
