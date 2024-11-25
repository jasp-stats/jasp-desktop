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

#ifndef ColumnTypesModel_H
#define ColumnTypesModel_H

#include <QAbstractListModel>
#include <QStringList>
#include "columntype.h"

///
/// A simple qt model with the columntypes, their respective icons and names
class ColumnTypesModel : public QAbstractListModel
{
	Q_OBJECT

public:
	enum {
		DisplayRole,
		NameRole,
		MenuImageSourceRole,
		JSFunctionRole,
		IsSeparatorRole,
		IsEnabledRole,
		TypeRole
	};

	ColumnTypesModel(QObject *parent, columnTypeVec types = {});

	int										rowCount(const QModelIndex &parent = QModelIndex())			const override	{	return _types.size();	}
	QVariant								data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	virtual QHash<int, QByteArray>			roleNames()													const override;

	Q_INVOKABLE	int							getType(int index)											const;
	void									setTypes(columnTypeVec types);
	bool									hasType(columnType type)									const;
	bool									hasAllTypes()												const;
	bool									hasMandatoryType()											const;
	columnType								defaultType()												const			{ return _defaultType;	}
	QStringList								iconList()													const;

private:
	static columnTypeVec					_allTypes;

	columnTypeVec							_types;
	columnType								_defaultType = columnType::unknown;

};

#endif  // ColumnTypesModel_H
