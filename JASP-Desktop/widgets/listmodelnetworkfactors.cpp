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

#include "listmodelnetworkfactors.h"
#include "analysis/options/options.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "utilities/qutils.h"
#include "log.h"

ListModelNetworkFactors::ListModelNetworkFactors(QMLListView* listView)
	: ListModel(listView)
{
	_itemType = "fixedFactors";
	initGroups({"Group 1", "Group 2"});
}

int ListModelNetworkFactors::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);
	return _groups.length();
}

QVariant ListModelNetworkFactors::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	
	if (row >= _groups.length())
	{
		Log::log()  << "Unknown row " << row << " in ListModelFactors. Length is " << _groups.length() << std::endl;
		return QVariant();
	}

	const Group& group = _groups.at(row);
	int groupIndex = row;

	QString value;
	if (role == Qt::DisplayRole || role == ListModelNetworkFactors::NameRole)
	{
		value = group.value;
	}
	else if (role == ListModelNetworkFactors::TypeRole)
	{
		QStringList listValues;
		if (group.isVirtual)
			listValues.push_back(tq("virtual"));

		listValues.push_back(tq("level"));
		if (groupIndex > 1 && !group.isVirtual)
			listValues.push_back(tq("deletable"));
		value = listValues.join(',');
	}

	return QVariant(value);
}

void ListModelNetworkFactors::initGroups(const std::vector<std::string> &groups)
{
	beginResetModel();
	_groups.clear();
	for (const std::string & group : groups)
		_groups.push_back(Group(group, _groups.length() > 2));

	// add an additional (virtual group) // TODO: see if we just want to call this "New Group"
	_groups.push_back(Group((tq("New Group").arg(_groups.length()+1)), true));
//	_groups.push_back(Group((tq("Group %1").arg(_groups.length()+1)), true));

	endResetModel();
	_terms.set(getGroups());
}

QString ListModelNetworkFactors::_removeGroup(int row)
{
	QString value;

	if (row >= _groups.length())
		return value;

	value = _groups[row].value;
	beginRemoveRows(QModelIndex(), row, row);
	_groups.removeAt(row);
	endRemoveRows();
	return value;
}

void ListModelNetworkFactors::itemChanged(int row, QVariant value)
{
	if (row >= _groups.length())
	{
		Log::log()  << "Index " << row << " in ListModelFactors is greater than the maximum " << _groups.length() << std::endl;
		return;
	}

	QString val = value.toString();
	Group&	group = _groups[row];

	if ((!group.isVirtual && group.value == val) || (group.isVirtual && val.isEmpty()))
		return;

	if (val.isEmpty() && !group.isVirtual)
		val = _removeGroup(row);

	if (!val.isEmpty())
	{
		// TODO: makeunique
		group.value = val;
		if (group.isVirtual)
		{
			group.isVirtual = false;
			beginInsertRows(QModelIndex(), row+1, row+1);
//			Group newGroup(tq("Group ") + QString::number(groupIndex + 2), true);
			Group newGroup(tq("New Group"), true);
			_groups.insert(row + 1, newGroup);
			endInsertRows();
		}
		QModelIndex modelIndex = index(row, 0);
		emit dataChanged(modelIndex, modelIndex);
	}

	_terms.set(getGroups());

	emit modelChanged();
}

void ListModelNetworkFactors::itemRemoved(int row)
{
	_removeGroup(row);
	_terms.set(getGroups());
	emit modelChanged();
}

std::vector<std::string> ListModelNetworkFactors::getGroups()
{
	std::vector<std::string> groups;
	for (const Group & g : _groups)
		if (!g.isVirtual)
			groups.push_back(g.value.toStdString());

	return groups;
}

