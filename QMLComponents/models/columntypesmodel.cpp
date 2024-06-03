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

#include "columntypesmodel.h"
#include "jasptheme.h"

ColumnTypesModel::ColumnTypesModel(QObject *parent, VectorType types) : QAbstractListModel(parent), _types{types}
{
	if (types.empty())
	{
		_types =  columnTypeToVector(); // If other types are added, they will be automatically included here.
		_types.erase(std::remove(_types.begin(), _types.end(), columnType::unknown), _types.end() );
		_types.erase(std::remove(_types.begin(), _types.end(), columnType::nominalText), _types.end() ); // Should be removed when nominalText is completely removed
	}
}

QVariant ColumnTypesModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	static std::map<columnType, QString> displayTexts =
	{
		std::make_pair ( columnType::scale,		QObject::tr("Scale")	) ,
		std::make_pair ( columnType::ordinal,	QObject::tr("Ordinal")	) ,
		std::make_pair ( columnType::nominal,	QObject::tr("Nominal")	)
	};

	static	std::map<columnType, QString> menuImageSources =
	{
		std::make_pair ( columnType::scale,		"variable-scale.svg"	),
		std::make_pair ( columnType::ordinal,	"variable-ordinal.svg"	),
		std::make_pair ( columnType::nominal,	"variable-nominal.svg"	)
	};

	switch(role)
	{
	case TypeRole:				return int(_types[index.row()]);
	case DisplayRole:			return displayTexts[_types[index.row()]];
	case MenuImageSourceRole:	return JaspTheme::currentIconPath() + menuImageSources[_types[index.row()]];
	case IsEnabledRole:			return true;
	case IsSeparatorRole:		return false;
	case JSFunctionRole:
	case NameRole:				return "";
	}

	return QVariant();
}


QHash<int, QByteArray> ColumnTypesModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray> {
		{	DisplayRole,			"displayText"		},
		{	NameRole,				"name"				},
		{	MenuImageSourceRole,	"menuImageSource"	},
		{	JSFunctionRole,			"jsFunction"		},
		{	IsSeparatorRole,		"isSeparator"		},
		{	IsEnabledRole,			"isEnabled"			},
		{	TypeRole,				"typeRole"			}
	};

	return roles;
}

int ColumnTypesModel::getType(int i) const
{
	return data(index(i, 0), TypeRole).toInt();
}

