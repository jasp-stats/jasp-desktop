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

#ifndef LISTMODELEXTRACONTROLS_H
#define LISTMODELEXTRACONTROLS_H

#include <QAbstractTableModel>
#include "common.h"

class ListModelAssignedInterface;
class BoundQMLItem;

class ListModelExtraControls : public QAbstractTableModel
{
	Q_OBJECT
public:
	enum ListModelExtraControlsRoles {
        NameRole = Qt::UserRole + 1,
		PathRole,
		TypeRole,
		PropertiesRole
    };
	
	ListModelExtraControls(ListModelAssignedInterface* parent, const QString& colName, const QVector<QMap<QString, QVariant> >& controlColumns);
	
	QHash<int, QByteArray>	roleNames()												const override;
	int						rowCount(const QModelIndex &parent = QModelIndex())		const override;
	int						columnCount(const QModelIndex &parent = QModelIndex())	const override { return 1; }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
	
	Q_INVOKABLE	void controlLoaded(const QString& name, QVariant item);
	Q_INVOKABLE void controlDestroyed(const QString& name, QVariant item);
	const QMap<QString, BoundQMLItem*>&	getBoundItems() const { return _boundItems; } 

private:
	struct ExtraColumnType {
		QString name;
		QString type;
		QString path;
		QMap<QString, QVariant> properties;
		
		ExtraColumnType(const QString& _name, const QString& _type, const QMap<QString, QVariant>& _properties)
			: name(_name), type(_type), path(_type + ".qml"), properties(_properties) {}		
	};

	ListModelAssignedInterface*		_assignedModel;
	QString							_colName;
	QMap<QString, ExtraColumnType*>	_extraColumns;
	QMap<QString, BoundQMLItem* >	_boundItems;
	QVector<QString>				_names;
};

#endif // LISTMODELEXTRACONTROLS_H
