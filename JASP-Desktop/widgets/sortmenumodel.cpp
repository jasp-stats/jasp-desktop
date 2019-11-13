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

#include "sortmenumodel.h"
#include "listmodelavailableinterface.h"
#include "qquick/jasptheme.h"

QMap<Sortable::SortType, QString> SortMenuModel::_labels; //Only set this constructor because otherwise it might cause a crash on Windows

SortMenuModel::SortMenuModel(QObject* parent, const QVector<Sortable::SortType> &menuEntries) : QAbstractListModel(parent)
{
	if(_labels.size() == 0)
		_labels =
		{
			{ Sortable::SortType::None,			tr("None") },
			{ Sortable::SortType::SortByName,	tr("Sort by name") },
			{ Sortable::SortType::SortByNameAZ, tr("Sort by name A-Z") },
			{ Sortable::SortType::SortByNameZA, tr("Sort by name Z-A") },
			{ Sortable::SortType::SortByType,	tr("Sort by type") },
			{ Sortable::SortType::SortByDate,	tr("Sort by date") },
			{ Sortable::SortType::SortBySize,	tr("Sort by size") }
		};


	_sortable = dynamic_cast<Sortable*>(parent);
	if (!_sortable)
	{
		QString errormsg(tr("SortMenuModel not called with a sortable parent"));
		throw new std::runtime_error(errormsg.toStdString().c_str());
	}

	for (const Sortable::SortType& sortType : menuEntries)
		_menuEntries.push_back(new SortMenuItem(sortType));

	_sortable->setSortModel(this);
}

QVariant SortMenuModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	SortMenuItem* entry = _menuEntries.at(index.row());

	switch(role)
	{
	case DisplayRole:				return _labels[entry->sortType];
	case MenuImageSourceRole:		return index.row() == _currentEntry ? JaspTheme::currentIconPath() + "check-mark.png" : "";
	case IsEnabledRole:				return true;
	default:						return QVariant();
	}
}


QHash<int, QByteArray> SortMenuModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{	DisplayRole,            "displayText"		},
		{	MenuImageSourceRole,    "menuImageSource"	},
		{	IsEnabledRole,			"isEnabled"			}
	};

	return roles;
}

void SortMenuModel::clickSortItem(int index)
{
	SortMenuItem* entry = _menuEntries[index];
	_sortable->sortItems(entry->sortType);

	_currentEntry = index;
}

void SortMenuModel::sortItems()
{
	SortMenuItem* entry = _menuEntries[_currentEntry];
	_sortable->sortItems(entry->sortType);
}

Sortable::SortType SortMenuModel::currentSortType()
{
	SortMenuItem* entry = _menuEntries[_currentEntry];
	return entry->sortType;
}

bool SortMenuModel::isAscending()
{
	SortMenuItem* entry = _menuEntries[_currentEntry];
	return entry->ascending;
}

void SortMenuModel::setCurrentEntry(Sortable::SortType sortType)
{
	int index = 0;
	for (const SortMenuItem* menuItem : _menuEntries)
	{
		if (menuItem->sortType == sortType)
			_currentEntry = index;
		index++;
	}
}
