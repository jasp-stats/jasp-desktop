//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "itemmodelselectitem.h"

#include "qutils.h"

using namespace std;

ItemModelSelectItem::ItemModelSelectItem()
{
	_boundTo = NULL;
	_selectedIndex = -1;
}

void ItemModelSelectItem::bindTo(Option *option)
{
	beginResetModel();

	_boundTo = dynamic_cast<OptionList *>(option);
	_selectedIndex = -1;

	if (_boundTo == NULL)
		return;

	vector<string> options = _boundTo->options();

	for (size_t i = 0; i < options.size(); i++)
	{
		if (options.at(i) == _boundTo->value())
		{
			_selectedIndex = i;
			break;
		}
	}

	endResetModel();
}

QVariant ItemModelSelectItem::data(const QModelIndex &index, int role) const
{
	if (_boundTo != NULL && role == Qt::CheckStateRole)
	{
		return index.row() == _selectedIndex;
	}
	else
	{
		return QStandardItemModel::data(index, role);
	}
}

bool ItemModelSelectItem::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (_boundTo != NULL && index.isValid() && role == Qt::CheckStateRole && value.canConvert(QVariant::Bool))
	{
		bool checked = value.toBool();

		if (checked)
		{
			setSelected(index.row());
		}
		else
		{
			if (_selectedIndex != -1)
			{
				int oldSelectedIndex = _selectedIndex;
				_selectedIndex = -1;
				_boundTo->setValue("");
				emit dataChanged(createIndex(oldSelectedIndex, 0), createIndex(oldSelectedIndex, 0), QVector<int>(1, Qt::CheckStateRole));
			}
		}

		return true;
	}
	else
	{
		return QStandardItemModel::setData(index, value, role);
	}
}

Qt::ItemFlags ItemModelSelectItem::flags(const QModelIndex &index) const
{
	if ( ! index.isValid())
		return Qt::ItemIsEnabled;

	return Qt::ItemIsUserCheckable | Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

int ItemModelSelectItem::selectedIndex()
{
	return _selectedIndex;
}

void ItemModelSelectItem::setSelected(int i)
{
	int oldSelectedIndex = _selectedIndex;
	_selectedIndex = i;
	string value = _boundTo->options().at(_selectedIndex);
	_boundTo->setValue(value);

	emit dataChanged(index(oldSelectedIndex, 0), index(oldSelectedIndex, 0), QVector<int>(1, Qt::CheckStateRole));
	emit dataChanged(index(_selectedIndex,   0), index(_selectedIndex,   0), QVector<int>(1, Qt::CheckStateRole));
}

