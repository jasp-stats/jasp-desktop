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

ListModelAssignedInterface::ListModelAssignedInterface(QMLListView* listView)
	: ListModelDraggable(listView)
  , _source(nullptr)
{
	connect(this, &QAbstractTableModel::modelReset, this, &ListModelAssignedInterface::modelResetHandler);
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

void ListModelAssignedInterface::modelResetHandler()
{
	if (!_extraControlsDefinitions.isEmpty())
	{
		QMapIterator<QString, ListModelExtraControls* > it(_extraControlsModels);
		while(it.hasNext())
		{
			it.next();
			_modelCache[it.key()] = it.value();
		}
		
		_extraControlsLoadedIndicator.clear();
		_extraControlsModels.clear();
		_rowNames.clear();
		for (int i = 0; i < rowCount(); i++)
		{
			QString colName = data(index(i, 0), ListModel::NameRole).toString();
			_extraControlsLoadedIndicator[colName] = _extraControlsNames;
			_rowNames[i] = colName;
			if (_modelCache.contains(colName))
				_extraControlsModels[colName] = _modelCache[colName];
			else
				_extraControlsModels[colName] = new ListModelExtraControls(this, colName, _extraControlsDefinitions);
		}
	}
}

void ListModelAssignedInterface::setAvailableModel(ListModelAvailableInterface *source)
{
	_source = source;
}

void ListModelAssignedInterface::addExtraControls(const QVector<QMap<QString, QVariant> > &extraControlColumns)
{
	_extraControlsDefinitions = extraControlColumns;
	
	for (const QMap<QString, QVariant>& extraControlDefinition : _extraControlsDefinitions)
		_extraControlsNames[extraControlDefinition["name"].toString()] = false;
}

void ListModelAssignedInterface::controlLoaded(const QString &colName, const QString &controlName)
{
	_extraControlsLoadedIndicator[colName][controlName] = true;
	bool allControlsLoaded = true;
	QMapIterator<QString, QMap<QString, bool> > it(_extraControlsLoadedIndicator);
	while (it.hasNext())
	{
		it.next();
		QMapIterator<QString, bool> it2(it.value());
		while (it2.hasNext())
		{
			it2.next();
			if (!it2.value())
				allControlsLoaded = false;
		}
	}
	if (allControlsLoaded)
		emit allExtraControlsLoaded();
}
