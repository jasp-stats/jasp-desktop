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
#include "ribbonmodel.h"


QVariant RibbonModel::data(const QModelIndex &index, int role) const {
	if (index.row() >= rowCount()) {
		return QVariant();
	}

	if		(role == ClusterRole)
		return QVariant();  // TODO
	else if	(role == DisplayRole)
		return QString::fromStdString(_moduleNames.at(index.row()));

	return QVariant();
}


QHash<int, QByteArray> RibbonModel::roleNames() const {

	static const auto roles = QHash<int, QByteArray>{
		{ ClusterRole,		"clusterMenu"	},
		{ DisplayRole,		"displayText"	}
	};

	return roles;
}


RibbonButtonModel* RibbonModel::ribbonButtonModel(std::string name) {

	auto it = std::find(_moduleNames.begin(), _moduleNames.end(), name);
	if (it != _moduleNames.end())
	{
		auto index = std::distance(_moduleNames.begin(), it);
		return _ribbonButtonModels.at(index);
	}
	//TODO: if it doesn't exist

	return NULL;
}


bool RibbonModel::isModuleName(std::string name) {

	if (std::find(_moduleNames.begin(), _moduleNames.end(), name) != _moduleNames.end()) {
		return true;
	}

	return false;
}
