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

#include "qmllistviewtermsavailable.h"
#include "listmodeltermsavailable.h"
#include "listmodelinteractionavailable.h"
#include "listmodelassignedinterface.h"

QMLListViewTermsAvailable::QMLListViewTermsAvailable(JASPControlBase* item, bool isInteraction)
	: JASPControlWrapper(item)
	, QMLListViewDraggable(item)

{
	if (isInteraction)	_availableModel = new ListModelInteractionAvailable(this);
	else				_availableModel = new ListModelTermsAvailable(this);

	_sortedMenuModel = new SortMenuModel(_availableModel, {Sortable::None, Sortable::SortByName, Sortable::SortByType});
}

void QMLListViewTermsAvailable::setUp()
{
	QMLListViewDraggable::setUp();
	setItemProperty("sortMenuModel", QVariant::fromValue(_sortedMenuModel));
}

void QMLListViewTermsAvailable::addAssignedModel(ListModelAssignedInterface *model)
{
	_assignedModels.push_back(model);

	connect(model, &ListModelDraggable::destroyed, this, &QMLListViewTermsAvailable::removeAssignedModel);

	 if (!_availableModel->areTermsVariables())		 model->listView()->setTermsAreNotVariables();
	 if (_availableModel->areTermsInteractions())	 model->listView()->setTermsAreInteractions();
}

void QMLListViewTermsAvailable::removeAssignedModel(ListModelDraggable* model)
{
	_assignedModels.removeAll(static_cast<ListModelAssignedInterface*>(model));
}

void QMLListViewTermsAvailable::setTermsAreNotVariables()
{
	QMLListView::setTermsAreNotVariables();

	for (ListModelAssignedInterface* model : _assignedModels)
		model->listView()->setTermsAreNotVariables();
}

void QMLListViewTermsAvailable::setTermsAreInteractions()
{
	QMLListView::setTermsAreInteractions();

	for (ListModelAssignedInterface* model : _assignedModels)
		model->listView()->setTermsAreInteractions();
}
