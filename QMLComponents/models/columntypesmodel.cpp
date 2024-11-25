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
#include "variableinfo.h"

columnTypeVec ColumnTypesModel::_allTypes;

ColumnTypesModel::ColumnTypesModel(QObject *parent, columnTypeVec types) : QAbstractListModel(parent)
{
	if (_allTypes.empty())
	{
		_allTypes =  columnTypeToVector();		// If other types are added, they will be automatically included here.
		_allTypes.erase(std::remove(_allTypes.begin(), _allTypes.end(), columnType::unknown),	 _allTypes.end() );
		_allTypes.erase(std::remove(_allTypes.begin(), _allTypes.end(), columnType::nominalText), _allTypes.end() ); // Should be removed when nominalText is completely removed
	}

	setTypes(types);
}

void ColumnTypesModel::setTypes(columnTypeVec types)
{
	if (types.empty())
		types = _allTypes;

	if (types == _types)
		return;

	beginResetModel();
	_types = types;
	_defaultType = _types.size() > 0 ? _types[0] : columnType::unknown;
	std::sort(_types.begin(), _types.end());
	endResetModel();
}

QVariant ColumnTypesModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	switch(role)
	{
	case TypeRole:				return int(_types[index.row()]);
	case DisplayRole:			return VariableInfo::getTypeFriendly(_types[index.row()]);
	case MenuImageSourceRole:	return VariableInfo::getIconFile(_types[index.row()], VariableInfo::IconType::DefaultIconType);
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

bool ColumnTypesModel::hasType(columnType type) const
{
	return std::find(_types.begin(), _types.end(), type) != _types.end();
}

bool ColumnTypesModel::hasAllTypes() const
{
	for (columnType type : _allTypes)
		if (!hasType(type))
			return false;

	return true;
}

bool ColumnTypesModel::hasMandatoryType() const
{
	return _types.size() == 1 && _types[0] != columnType::unknown;
}

QStringList ColumnTypesModel::iconList() const
{
	QStringList result;
	if (_types == _allTypes)
		return result;

	for (columnType type : _types)
		result.push_back(VariableInfo::getIconFile(type, VariableInfo::IconType::InactiveIconType));

	return result;
}



