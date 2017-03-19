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

#include "tablemodelanovamodelnuisancefactors.h"

using namespace std;

TableModelAnovaModelNuisanceFactors::TableModelAnovaModelNuisanceFactors(QObject *parent)
	: TableModelAnovaModel(parent)
{
	_nuisanceOption = NULL;
}

int TableModelAnovaModelNuisanceFactors::columnCount(const QModelIndex &parent) const
{
	return 2;
}

Qt::ItemFlags TableModelAnovaModelNuisanceFactors::flags(const QModelIndex &index) const
{
	if (index.isValid() && index.column() == 1)
		return Qt::ItemIsUserCheckable | Qt::ItemIsEnabled | Qt::ItemIsEditable;
	else
		return TableModelAnovaModel::flags(index);
}

QVariant TableModelAnovaModelNuisanceFactors::data(const QModelIndex &index, int role) const
{
	if (index.column() == 0)
	{
		return TableModelAnovaModel::data(index, role);
	}
	else
	{
		if (role == Qt::CheckStateRole)
			return QVariant(_nuisance.at(index.row()) ? 2 : 0);
		else if (role == Qt::EditRole)
			return QVariant(true);
		else if (role == Qt::SizeHintRole)
			return QVariant(QSize(50, -1));
		else
			return QVariant();
	}
}

QVariant TableModelAnovaModelNuisanceFactors::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (role == Qt::DisplayRole && orientation == Qt::Horizontal)
	{
		if (section == 0)
			return "Model Terms";
		else if (section == 1)
			return "Is Nuisance";
	}

	return QVariant();
}

void TableModelAnovaModelNuisanceFactors::assignToNuisanceOption()
{
	/*if (_nuisanceOption != NULL)
	{
		vector<string> values;

		QList<bool>::Iterator isNuisanceItr = _nuisance.begin();
		QList<QList<ColumnInfo> >::iterator termIterator = _terms.begin();

		while (isNuisanceItr != _nuisance.end())
		{
			if (*isNuisanceItr)
			{
				QString term = itemsToString(*termIterator);
				QByteArray bytes = term.toUtf8();
				string v(bytes.constData(), bytes.length());
				values.push_back(v);
			}

			isNuisanceItr++;
			termIterator++;
		}

		_nuisanceOption->setValue(values);
	}*/
}

void TableModelAnovaModelNuisanceFactors::setNuisanceTermsOption(OptionVariables *nuisanceOption)
{
	_nuisanceOption = nuisanceOption;
}

void TableModelAnovaModelNuisanceFactors::mimeDataMoved(const QModelIndexList &indexes)
{
	/*beginResetModel();

	QModelIndexList sorted = indexes;

	int lastRowDeleted = -1;

	qSort(sorted.begin(), sorted.end(), qGreater<QModelIndex>());

	foreach (const QModelIndex &index, sorted)
	{
		int row = index.row();
		if (row != lastRowDeleted)
		{
			_terms.removeAt(row);
			_nuisance.removeAt(row);
		}
		lastRowDeleted = row;
	}

	endResetModel();

	assignToOption();*/
}

bool TableModelAnovaModelNuisanceFactors::insertRows(int row, int count, const QModelIndex &parent)
{
	/*beginInsertRows(parent, row, row + count - 1);

	for (int i = 0; i < count; i++)
		_terms.insert(row, QList<ColumnInfo>());

	for (int i = 0; i < count; i++)
		_nuisance.insert(row, false);

	endInsertRows();*/

	return true;
}

bool TableModelAnovaModelNuisanceFactors::removeRows(int row, int count, const QModelIndex &parent)
{
	beginRemoveRows(parent, row, row + count - 1);

	for (int i = 0; i < count; i++)
		_nuisance.removeAt(row);

	//for (int i = 0; i < count; i++)
	//	_terms.removeAt(row);

	endRemoveRows();

	assignToNuisanceOption();

	return true;
}

bool TableModelAnovaModelNuisanceFactors::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (role == Qt::CheckStateRole && index.column() == 1)
	{
		_nuisance[index.row()] = (value.value<int>() == 2);

		QVector<int> roles;
		roles << role;

		emit dataChanged(index, index, roles);

		assignToNuisanceOption();

		return true;
	}

	return TableModelAnovaModel::setData(index, value, role);
}
