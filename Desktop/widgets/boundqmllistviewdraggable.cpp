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

#include "boundqmllistviewdraggable.h"
#include "../analysis/analysisform.h"
#include "listmodelassignedinterface.h"
#include "qmllistviewtermsavailable.h"
#include "analysis/options/optionstable.h"
#include "analysis/options/optionvariable.h"
#include "analysis/options/optionboolean.h"
#include "analysis/options/optionlist.h"
#include "analysis/options/optionstring.h"

#include <QQuickItem>

BoundQMLListViewDraggable::BoundQMLListViewDraggable(JASPControl *item)
	: QMLListViewDraggable(item)
	, BoundQMLItem()
{
}

void BoundQMLListViewDraggable::setUp()
{	
	QMLListViewDraggable::setUp();
	
	ListModel* availableModel = form()->getRelatedModel(this);
	ListModelAssignedInterface* _assignedModel = assignedModel();
	
	if (!availableModel)
	{
		if (sourceModels().empty() && !getItemProperty("debug").toBool())
			addControlError(tr("Cannot find source for VariableList %1").arg(name()));
	}
	else
	{
		_availableModel = dynamic_cast<ListModelAvailableInterface*>(availableModel);
		if (!_availableModel)
			addControlError(tr("Wrong kind of source for VariableList %1").arg(name()));
		else
		{
			_assignedModel->setAvailableModel(_availableModel);
			QMLListViewTermsAvailable* qmlAvailableListView = dynamic_cast<QMLListViewTermsAvailable*>(_availableModel->listView());
			if (qmlAvailableListView)
			{
				qmlAvailableListView->addAssignedModel(_assignedModel);
				addDependency(qmlAvailableListView);
			}
			connect(_availableModel, &ListModelAvailableInterface::allAvailableTermsChanged, _assignedModel, &ListModelAssignedInterface::availableTermsChanged);
		}
	}	
}

ListModelAssignedInterface* BoundQMLListViewDraggable::assignedModel()
{
	return dynamic_cast<ListModelAssignedInterface*>(model());
}
