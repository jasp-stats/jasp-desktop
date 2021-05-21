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


#include "menumodel.h"
#include "modules/ribbonbutton.h"


MenuModel::MenuModel(RibbonButton *parent, Modules::DynamicModule * module)
	: QAbstractListModel(parent), _ribbonButton(parent), _module(module)
{

}

MenuModel::MenuModel(RibbonButton * parent, Modules::AnalysisEntries * entries)
: QAbstractListModel(parent), _ribbonButton(parent), _entries(entries)
{
	for(const auto * entry : *_entries)
		if(entry->icon() != "")
			_hasIcons = true;
}

QVariant MenuModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	Modules::AnalysisEntry * entry =  analysisEntries().at(index.row());

	switch(role)
	{
	case DisplayRole:				return QString::fromStdString(entry->menu());
	case AnalysisFunctionRole:		return QString::fromStdString(entry->function());
	case MenuImageSourceRole:		return QString::fromStdString(entry->icon());
	case IsSeparatorRole:			return entry->isSeparator();
	case isGroupTitleRole:			return entry->isGroupTitle();
	case IsEnabledRole:				return entry->isEnabled() && (!entry->requiresData() || _ribbonButton->dataLoaded());
	default:						return QVariant();
	}
}


QHash<int, QByteArray> MenuModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{	DisplayRole,            "displayText"		},
		{	AnalysisFunctionRole,   "analysisEntry"		},
		{	MenuImageSourceRole,    "menuImageSource"	},
		{	IsSeparatorRole,		"isSeparator"		},
		{	isGroupTitleRole,		"isGroupTitle"		},
		{	IsEnabledRole,			"isEnabled"			}
	};

	return roles;
}

Modules::AnalysisEntry *MenuModel::getAnalysisEntry(const std::string& func)
{
	for (Modules::AnalysisEntry* analysis : analysisEntries())
	{
		if (analysis->function() == func)
			return analysis;
	}

	return nullptr;
}

const std::vector<Modules::AnalysisEntry*> &	MenuModel::analysisEntries() const
{
	return _module ? _module->menu() : *_entries;
}

bool MenuModel::isAnalysisEnabled(int index)
{
	return analysisEntries().at(index)->isEnabled() && (!analysisEntries().at(index)->requiresData() || _ribbonButton->dataLoaded());
}
