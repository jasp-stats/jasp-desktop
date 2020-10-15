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
#include "utilities/qutils.h"
#include "utilities/settings.h"
#include "qquick/jasptheme.h"

QVariant ColumnTypesModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

			QStringList displayTexts		= { tr("Scale"),			tr("Ordinal"),			tr("Nominal")			};
	static	QStringList menuImageSources	= { "variable-scale.png",	"variable-ordinal.png", "variable-nominal.png"	};

	switch(role)
	{
	case DisplayRole:			return displayTexts[index.row()];
	case MenuImageSourceRole:	return JaspTheme::currentIconPath() + menuImageSources[index.row()];
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
		{	IsEnabledRole,			"isEnabled"			}
	};

	return roles;
}

