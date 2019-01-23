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

#include <QDebug>
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
	}
}

QHash<int, QByteArray> ListModelExtraControls::roleNames() const
{
	QHash<int, QByteArray> roles;
	roles[NameRole] = "name";
	roles[PathRole] = "path";
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
	{
		return QVariant(_extraColumns[_names[row]]->name);
	}	
	else if (role == ListModelExtraControls::PathRole)
	{
		return QVariant(_extraColumns[_names[row]]->path);
	}
	
	return QVariant();
}

void ListModelExtraControls::controlLoaded(const QString& name, QVariant item)
{
	QQuickItem *quickItem = qobject_cast<QQuickItem *>(item.value<QObject *>());
	if (quickItem)
	{
		ExtraColumnType* extraColumn = _extraColumns[name];
		if (extraColumn)
		{			
			if (_boundItems.contains(name))
			{
				BoundQMLItem* boundItem = _boundItems[name];
				boundItem->resetQMLItem(quickItem);
			}
			else
			{
				QMapIterator<QString, QVariant> i(extraColumn->properties);
				while (i.hasNext())
				{
					i.next();
					quickItem->setProperty(i.key().toStdString().c_str(), i.value());
				}
				qmlControlType controlType = qmlControlTypeFromQString(extraColumn->type);
				BoundQMLItem* boundItem = nullptr;
				switch(controlType)
				{
				case qmlControlType::CheckBox:		//fallthrough:
				case qmlControlType::Switch:		boundItem = new BoundQMLCheckBox(quickItem,	_assignedModel->listView()->form());	break;
				case qmlControlType::TextField:		boundItem = new BoundQMLTextInput(quickItem, _assignedModel->listView()->form());	break;
				case qmlControlType::ComboBox:		boundItem = new BoundQMLComboBox(quickItem,	_assignedModel->listView()->form());	break;
				default:
					qDebug() << "Control type " << extraColumn->type << " not supported in TableView";
				}
				
				if (boundItem)
				{
					boundItem->setUp();
					_boundItems[name] = boundItem;
				}
	
			}
			
			_assignedModel->controlLoaded(_colName, name);
		}
		else
			_assignedModel->addError("Cannot find Extra column with name " + name);
	}
	else
		qDebug() << "Quick Item not found";
	
}
