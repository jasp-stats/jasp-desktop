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
#include "modules/ribbonbutton.h"


AnalysisMenuModel::AnalysisMenuModel(RibbonButton *parent) : QAbstractListModel(parent), _ribbonButton(parent)
{

}

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
		return entry->icon().empty() ? QString() : (QString::fromStdString((_ribbonButton->isDynamic() ? "file:" : "qrc:/icons/") + entry->icon()));
	else if (role == IsSeparatorRole)
		return entry->isSeparator();
	else if (role == isGroupTitleRole)
		return entry->isGroupTitle();

	return QVariant();
}


QHash<int, QByteArray> AnalysisMenuModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{	DisplayRole,            "displayText"		},
		{	AnalysisFunctionRole,   "analysisEntry"		},
		{	MenuImageSourceRole,    "menuImageSource"	},
		{	IsSeparatorRole,		"isSeparator"		},
		{	isGroupTitleRole,		"isGroupTitle"		}
	};

	return roles;
}

void AnalysisMenuModel::setAnalysisEntries(const std::vector<Modules::AnalysisEntry *> &analysisEntries)
{
	_analysisEntries.clear();

	Modules::AnalysisEntry* previousEntry = nullptr;
	for (Modules::AnalysisEntry* entry: analysisEntries)
	{
		if (entry->isGroupTitle())
		{
			if (previousEntry && !previousEntry->isSeparator() )
				_analysisEntries.push_back(new Modules::AnalysisEntry());
		}
		_analysisEntries.push_back(entry);
		previousEntry = entry;
	}
}

Modules::AnalysisEntry *AnalysisMenuModel::getAnalysisEntry(const std::string& name)
{
	for (Modules::AnalysisEntry* analysis : _analysisEntries)
	{
		if (analysis->function() == name)
			return analysis;
	}
	
	return nullptr;
}
