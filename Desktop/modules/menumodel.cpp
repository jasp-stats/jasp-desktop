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
	_setEntries(module->menu());

	connect(module, &Modules::DynamicModule::descriptionReloaded, this, [this](Modules::DynamicModule * module) {_setEntries(module->menu()); });
}

MenuModel::MenuModel(RibbonButton * parent, const Modules::AnalysisEntries & entries)
: QAbstractListModel(parent), _ribbonButton(parent)
{
	_setEntries(entries);
}

MenuModel::MenuModel(RibbonButton *parent, Modules::DynamicModule * module, const Modules::AnalysisEntries & entries, bool isSubMenu)
	: QAbstractListModel(parent), _ribbonButton(parent), _module(module), _isSubMenu(isSubMenu)
{
	_setEntries(entries);
}

void MenuModel::_setEntries(const Modules::AnalysisEntries & entries)
{
	_subMenus.clear();
	if (_module && _module->useSubMenus() && !_isSubMenu)
	{
		// Keep only the main entries, and add submenus
		Modules::AnalysisEntries mainEntries, subEntries;
		QString currentGroupTitle;
		// If the first items are not group items, then add them to the main menu.
		// When a group item is found, all items afterwards are set in a submenu, until another group item is found.
		for(auto * entry : entries)
		{
			if (entry->isGroupTitle())
			{
				mainEntries.push_back(entry);	// Add the Group to the main menu
				if (!currentGroupTitle.isEmpty())
				{
					// The group is complete: add a subMenu
					_subMenus[currentGroupTitle] = new MenuModel(_ribbonButton, _module, subEntries, true);
					subEntries.clear();
				}
				currentGroupTitle = tq(entry->title());
			}
			else if (currentGroupTitle.isEmpty())
				mainEntries.push_back(entry); // No group item found yet: add it to the main menu.
			else
				subEntries.push_back(entry);
		}
		_entries = mainEntries;

		if (!subEntries.empty() && !currentGroupTitle.isEmpty())
			_subMenus[currentGroupTitle] = new MenuModel(_ribbonButton, _module, subEntries, true);
	}
	else
		_entries = entries;

	_hasIcons = false;
	for(const auto * entry : _entries)
		if(entry->icon() != "" && !entry->isGroupTitle())
			_hasIcons = true;
}


void MenuModel::setDynamicModule(Modules::DynamicModule *module)
{
	if (_module == module)
		return;

	beginResetModel();

	_module = module;
	_setEntries(module->menu());
	connect(module, &Modules::DynamicModule::descriptionReloaded, this, [this](Modules::DynamicModule * module) {_setEntries(module->menu()); });

	endResetModel();
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
	case isSmallRole:				return entry->smallIcon();
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
		{	IsEnabledRole,			"isEnabled"			},
		{	isSmallRole,			"isSmall"			}
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

QVariant MenuModel::getSubMenu(int index) const
{
	if (index < 0 || index >= analysisEntries().size())
		return QVariant();

	QString title = getAnalysisTitle(index);
	if (_subMenus.count(title))
		return QVariant::fromValue(_subMenus.at(title));

	return QVariant();
}

const std::vector<Modules::AnalysisEntry*> &	MenuModel::analysisEntries() const
{
	return _entries;
}

bool MenuModel::isAnalysisEnabled(int index)
{
	return analysisEntries().at(index)->isEnabled() && (!analysisEntries().at(index)->requiresData() || _ribbonButton->dataLoaded());
}
