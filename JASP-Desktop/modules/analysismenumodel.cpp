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

#include <QDebug>
#include "analysismenumodel.h"


QVariant AnalysisMenuModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	Modules::AnalysisEntry* entry = _analysisEntries.at(index.row());

	if	(role == DisplayRole)
		return QString::fromStdString(entry->title());
	else if (role == AnalysisFunctionRole)
		return QString::fromStdString(entry->function());
	else if (role == MenuImageSourceRole)
		return "";  // We don't need image for analysis menu. TODO: Move this to (parent) MenuModel

	return QVariant();
}


QHash<int, QByteArray> AnalysisMenuModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{	DisplayRole,            "displayText"		},
		{	AnalysisFunctionRole,   "analysisEntry"		},
		{	MenuImageSourceRole,    "menuImageSource"	}
	};

	return roles;
}
