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
#include "listmodelextracontrols.h"
#include "boundqmllistviewterms.h"
#include <QTimer>

ListModelAssignedInterface::ListModelAssignedInterface(QMLListView* listView)
	: ListModelDraggable(listView)
  , _source(nullptr)
{
}


QVariant ListModelAssignedInterface::data(const QModelIndex &index, int role) const
{
	if (role == ListModel::ExtraColumnsRole)
	{
		int row = index.row();
		return QVariant::fromValue(_extraControlsModels[_rowNames[row]]);
	}
	else
		return ListModelDraggable::data(index, role);
	
}

void ListModelAssignedInterface::endResetModel()
{
	addExtraControlModels();
	ListModelDraggable::endResetModel();
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

void ListModelAssignedInterface::addExtraControlModels()
{
	if (!_extraControlsDefinitions.isEmpty())
	{
		_extraControlsModels.clear();
		_rowNames.clear();
		for (int i = 0; i < rowCount(); i++)
		{
			QString colName = data(index(i, 0), ListModel::NameRole).toString();
			_rowNames[i] = colName;
			if (_modelCache.contains(colName))
				_extraControlsModels[colName] = _modelCache[colName];
			else
			{
				ListModelExtraControls* extraControlsModel = new ListModelExtraControls(this, _extraControlsDefinitions);
				_extraControlsModels[colName] = extraControlsModel;
				_modelCache[colName] = extraControlsModel;
			}
		}
	}
}

// This function is called by initTerms for models that may have extra columns
void ListModelAssignedInterface::initExtraControlTerms()
{
	BoundQMLListViewTerms* listViewTerms = dynamic_cast<BoundQMLListViewTerms*>(listView());
	if (listViewTerms)
		// initTerms calls begin/endResetModel that will build the QML items in the List View.
		// We must wait that these QML Items are completely built so that we can bind the extra controls (if they exist)
		QTimer::singleShot(0, listViewTerms, &BoundQMLListViewTerms::bindExtraControlOptions);
}

void ListModelAssignedInterface::setAvailableModel(ListModelAvailableInterface *source)
{
	_source = source;
}

void ListModelAssignedInterface::addExtraControls(const QVector<QMap<QString, QVariant> > &extraControlColumns)
{
	_extraControlsDefinitions = extraControlColumns;	
}
