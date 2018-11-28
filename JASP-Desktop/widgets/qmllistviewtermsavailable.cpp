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
#include "listmodelassignedinterface.h"
#include <QQuickItem>
#include <QQmlProperty>

QMLListViewTermsAvailable::QMLListViewTermsAvailable(QQuickItem* item, AnalysisQMLForm* form)
	: QMLItem(item, form)
	, QMLListViewDraggable(item, form)
{
	_availableModel = new ListModelTermsAvailable(this);
}

void QMLListViewTermsAvailable::addAssignedModel(ListModelAssignedInterface *model)
{
	_assignedModels.push_back(model);
	 if (!_availableModel->areTermsVariables())
		 model->listView()->setTermsAreNotVariables();
}

void QMLListViewTermsAvailable::setTermsAreNotVariables()
{
	QMLListView::setTermsAreNotVariables();
	for (ListModelAssignedInterface* model : _assignedModels)
		model->listView()->setTermsAreNotVariables();
}
