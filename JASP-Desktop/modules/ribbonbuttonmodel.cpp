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
#include "ribbonbuttonmodel.h"


QVariant RibbonButtonModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	Modules::RibbonEntry* entry = _ribbonEntries.at(index.row());
    AnalysisMenuModel *menuModel = qobject_cast<AnalysisMenuModel *>(_analysisMenuModels.at(index.row()));

	if		(role == AnalysisMenuRole)
        return QVariant::fromValue(menuModel);
	else if	(role == DisplayRole)
		return QString::fromStdString(entry->title());
	else if	(role == IconSourceRole)
		return QString::fromStdString(entry->icon());

	return QVariant();
}


QHash<int, QByteArray> RibbonButtonModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{	AnalysisMenuRole,	"analysisMenu"	},
		{	DisplayRole,		"displayText"	},
		{	IconSourceRole,		"iconSource"	}
	};

	return roles;
}

void RibbonButtonModel::setRibbonEntries(std::vector<Modules::RibbonEntry*> ribbonEntries)
{
    _ribbonEntries = ribbonEntries;
    AnalysisMenuModel *model;

    for (auto ribbonEntry : _ribbonEntries) {
        model = new AnalysisMenuModel(this);
        model->setAnalysisEntries(ribbonEntry->analysisEntries());
        _analysisMenuModels.push_back(model);
    }
}
