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
#include "boundqmlcombobox.h"
#include "boundqmlcheckbox.h"
#include "boundqmltextinput.h"

#include "log.h"

#include <QQuickItem>

ListModelExtraControls::ListModelExtraControls(ListModelAssignedInterface* parent, const QString& colName, const QVector<QMap<QString, QVariant> >& controlColumns) 
	: QAbstractTableModel(parent), _assignedModel(parent), _colName(colName)
{
	for (QMap<QString, QVariant> controlColumn: controlColumns)
	{
		QString name = controlColumn["name"].toString();
		QString type = controlColumn["type"].toString();
		
		controlColumn.remove("type");
		
		if (name.isEmpty())
			_assignedModel->addError(QString::fromLatin1("An Extra column in ") + parent->name() + QString::fromLatin1(" has no name"));
		else if (type.isEmpty())
			_assignedModel->addError(QString::fromLatin1("An Extra column in ") + parent->name() + QString::fromLatin1(" has no type"));
		else
		{
			_extraColumns[name] = new ExtraColumnType(name, type, controlColumn);
			_names.push_back(name);
		}


		BoundQMLItem* boundItem = nullptr;
		if (type == "CheckBox" || type == "Switch")
			boundItem = new BoundQMLCheckBox(controlColumn,	_assignedModel->listView()->form());
		else if (type == "ComboBox" || type == "Dropdown")
			boundItem = new BoundQMLComboBox(controlColumn,	_assignedModel->listView()->form());
		else if (type == "TextField" || type == "IntegerField" || type == "DoubleField" || type == "PercentField")
		{
			QString inputType = "string";
			if (type == "IntegerField")
				inputType = "integer";
			else if (type == "DoubleField")
				inputType = "double";
			else if (type == "PercentField")
				inputType = "percent";

			controlColumn["inputType"] = inputType;
			boundItem = new BoundQMLTextInput(controlColumn,  _assignedModel->listView()->form());
		}
		else
			Log::log() << "Control type " << type.toStdString() << " not supported in TableView" << std::flush;

		if (boundItem)
		{
			boundItem->setUp();
			_boundItems[name] = boundItem;
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

void ListModelExtraControls::controlLoaded(const QString& name, QVariant item)
{
	QQuickItem *quickItem = qobject_cast<QQuickItem *>(item.value<QObject *>());
	if (quickItem)
	{
		if (_boundItems.contains(name))
		{
			BoundQMLItem* boundItem = _boundItems[name];
			boundItem->resetQMLItem(quickItem);
		}
		else
			Log::log() << "controlLoaded: Cannot find bound item " << name.toStdString() << std::flush;
	}
	else
		Log::log() << "Quick Item not found" << std::flush;

}

void ListModelExtraControls::controlDestroyed(const QString &name, QVariant item)
{
	QQuickItem *quickItem = qobject_cast<QQuickItem *>(item.value<QObject *>());
	if (_boundItems.contains(name))
	{
		BoundQMLItem* boundItem = _boundItems[name];
		if (boundItem->item() == quickItem)
			boundItem->resetQMLItem(nullptr);
	}
	else
		Log::log() << "controlDestroyed: Cannot find bound item " << name.toStdString() << std::flush;

}
