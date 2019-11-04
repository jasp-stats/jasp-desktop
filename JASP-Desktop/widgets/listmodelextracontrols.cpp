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

#include "listmodelextracontrols.h"
#include "listmodelassignedinterface.h"
#include "boundqmllistviewdraggable.h"
#include "boundqmllistviewterms.h"
#include "boundqmlcombobox.h"
#include "boundqmlcheckbox.h"
#include "boundqmltextinput.h"
#include "analysis/analysisform.h"

#include "log.h"

#include <QQuickItem>

ListModelExtraControls::ListModelExtraControls(ListModel* parent, const QVector<QMap<QString, QVariant> >& controlColumns)
	: QAbstractTableModel(parent), _parentModel(parent)
{
	for (QMap<QString, QVariant> controlColumn: controlColumns)
	{
		QString name = controlColumn["name"].toString();
		QString type = controlColumn["type"].toString();
		
		controlColumn.remove("type");
		
		if (name.isEmpty())
			_parentModel->addError(QString::fromLatin1("An Extra column in ") + parent->name() + QString::fromLatin1(" has no name"));
		else if (type.isEmpty())
			_parentModel->addError(QString::fromLatin1("An Extra column in ") + parent->name() + QString::fromLatin1(" has no type"));
		else
		{
			_extraColumns[name] = new ExtraColumnType(name, type, controlColumn);
			_names.push_back(name);
		}
	}
}

QHash<int, QByteArray> ListModelExtraControls::roleNames() const
{
	QHash<int, QByteArray> roles;
	roles[NameRole] = "name";
	roles[PathRole] = "path";
	roles[TypeRole] = "type";
	roles[PropertiesRole] = "properties";

	return roles;
}

int ListModelExtraControls::rowCount(const QModelIndex &) const
{
	return _extraColumns.size();
}

QVariant ListModelExtraControls::data(const QModelIndex &index, int role) const
{
	int row = index.row();

	if (role == Qt::DisplayRole || role == ListModelExtraControls::NameRole)
		return QVariant(_extraColumns[_names[row]]->name);
	else if (role == ListModelExtraControls::PathRole)
		return QVariant(_extraColumns[_names[row]]->path);
	else if (role == ListModelExtraControls::TypeRole)
		return QVariant(_extraColumns[_names[row]]->type);
	else if (role == ListModelExtraControls::PropertiesRole)
		return QVariant(_extraColumns[_names[row]]->properties);

	return QVariant();
}

void ListModelExtraControls::controlLoaded(QString name, QVariant item)
{
	QQuickItem *quickItem = qobject_cast<QQuickItem *>(item.value<QObject *>());
	if (quickItem)
	{
		BoundQMLItem* boundItem = nullptr;
		if (_boundItems.contains(name))
		{
			boundItem = _boundItems[name];
			boundItem->resetQMLItem(quickItem);
		}
		else
		{
			qmlControlType controlType;
			boundItem = dynamic_cast<BoundQMLItem*>(_parentModel->listView()->form()->buildQMLItem(quickItem, controlType));
			if (boundItem)
			{
				boundItem->setUp();
				QString controlName = boundItem->name();
				_boundItems[controlName] = boundItem;
			}
			else
				Log::log() << "controlLoaded: Wrong type of control : " << qmlControlTypeToString(controlType) << std::endl;
		}
	}
	else
		Log::log() << "Quick Item not found" << std::endl;

}

void ListModelExtraControls::controlsDestroyed()
{
	for (auto it : _boundItems)
		it->resetQMLItem(nullptr);
}
