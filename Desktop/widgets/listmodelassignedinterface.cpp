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

#include "listmodelassignedinterface.h"

ListModelAssignedInterface::ListModelAssignedInterface(QMLListView* listView)
	: ListModelDraggable(listView)
  , _source(nullptr)
{
}

void ListModelAssignedInterface::refresh()
{
	bool doRefresh = true;
	QList<int> toRemove;
	for (int i = 0; i < rowCount(); i++)
	{
		QString term = data(index(i, 0)).toString();
		if (!isAllowed(term))
			toRemove.push_back(i);
	}

	if (toRemove.count() > 0)
	{
		QMLListViewDraggable* qmlListView = dynamic_cast<QMLListViewDraggable*>(listView());
		if (qmlListView)
		{
			qmlListView->moveItems(toRemove, _source);
			doRefresh = false;
		}
	}

	if (doRefresh)
		ListModelDraggable::refresh();
}

void ListModelAssignedInterface::setAvailableModel(ListModelAvailableInterface *source)
{
	_source = source;
}
