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

#include "datasettablemodel.h"

#include <iostream>
#include <fstream>

#include <QSize>
#include <QDebug>
#include <QQmlEngine>

#include "qutils.h"

using namespace std;

DataSetTableModel::DataSetTableModel(QObject *parent) :
    QAbstractTableModel(parent)
{
	_dataSet = NULL;

	_nominalTextIcon = QIcon(":/icons/variable-nominal-text.svg");
	_nominalIcon = QIcon(":/icons/variable-nominal.svg");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.svg");
	_scaleIcon = QIcon(":/icons/variable-scale.svg");
}

QVariant DataSetTableModel::getColumnTypesWithCorrespondingIcon(bool BothNominalVersions)
{
	QVariantList ColumnTypeAndIcons;

	ColumnTypeAndIcons.push_back(QVariant(QString("../icons/variable-scale.svg")));
	ColumnTypeAndIcons.push_back(QVariant(QString("../icons/variable-ordinal.svg")));
	ColumnTypeAndIcons.push_back(QVariant(QString("../icons/variable-nominal.svg")));
	if(BothNominalVersions)
		ColumnTypeAndIcons.push_back(QVariant(QString("../icons/variable-nominal-text.svg")));

	return QVariant(ColumnTypeAndIcons);
}

void DataSetTableModel::setDataSet(DataSet* dataSet)
{
    beginResetModel();
	_dataSet = dataSet;
    endResetModel();
}


int DataSetTableModel::rowCount(const QModelIndex &parent) const
{
	if (_dataSet == NULL)
		return 0;

	return parent.isValid() ? 0 : _dataSet->rowCount();
}

int DataSetTableModel::columnCount(const QModelIndex &parent) const
{
	if (_dataSet == NULL)
		return 0;

	return parent.isValid() ? 0 : _dataSet->columnCount();
}

QVariant DataSetTableModel::data(const QModelIndex &index, int role) const
{
	if (_dataSet == NULL)
		return QVariant();

	int column = -1;

	if (role == Qt::DisplayRole)
		column = index.column();
	else if(role >= Qt::UserRole)
		column = role - Qt::UserRole;

	if(column > -1)
	{
		QString value = tq(_dataSet->column(column)[index.row()]);
		return QVariant(value);
	}

    return QVariant();
}

QVariant DataSetTableModel::columnTitle(int column) const
{
	if(column >= 0)
	{
		QString value = tq(_dataSet->column(column).name());
		return QVariant(value);
	}
	else
		return QVariant();
}

QVariant DataSetTableModel::columnIcon(int column) const
{
	if(column >= 0)
	{
		Column &columnref = _dataSet->column(column);
		return QVariant(columnref.columnType());
	}
	else
		return QVariant();
}

QVariant DataSetTableModel::headerData ( int section, Qt::Orientation orientation, int role) const
{
	if (_dataSet == NULL)
		return QVariant();

	if (role == Qt::DisplayRole)
	{
		if (orientation == Qt::Horizontal)
		{
			QString value = tq(_dataSet->column(section).name()) + QString("        ");
			return QVariant(value);
		}
		else
		{
			return QVariant(section + 1);
		}
	}
	else if (role == Qt::DecorationRole && orientation == Qt::Horizontal)
	{
		Column &column = _dataSet->column(section);

		switch (column.columnType())
		{
		case Column::ColumnTypeNominalText:
			return QVariant(_nominalTextIcon);
		case Column::ColumnTypeNominal:
			return QVariant(_nominalIcon);
		case Column::ColumnTypeOrdinal:
			return QVariant(_ordinalIcon);
		case Column::ColumnTypeScale:
			return QVariant(_scaleIcon);
		default:
			return QVariant();
		}
	}
	else if (role == Qt::SizeHintRole && orientation == Qt::Vertical)
	{
		return QVariant(/*QSize(80, -1)*/);
	}
	else if (role == Qt::TextAlignmentRole)
	{
		return QVariant(Qt::AlignCenter);
	}

	return QVariant();
}

QHash<int, QByteArray> DataSetTableModel::roleNames() const
{
	QHash<int, QByteArray> roles = QAbstractTableModel::roleNames ();

	for(int i=0; i<columnCount(); i++)
		roles[Qt::UserRole + i] = (QString("column_")+QString::number(i)).toUtf8();

	return roles;
}

QStringList DataSetTableModel::userRoleNames() const
{
	QMap<int, QString> res;
	QHashIterator<int, QByteArray> i(roleNames());
	while (i.hasNext()) {
		i.next();
		if(i.key() >= Qt::UserRole)
			res[i.key()] = i.value();
	}
	return res.values();
}

bool DataSetTableModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	/*if (_dataSet == NULL)
		return false;

	bool ok;

	Column &column = _dataSet->columns()[index.column()];
	if (column.dataType() == Column::DataTypeInt)
	{
		int v = value.toInt(&ok);
		if (ok)
			column.setValue(index.row(), v);
		else
			emit badDataEntered(index);

		return ok;
	}*/

	//_dataSet->columns()[index.column()].setValue(index.row(), v);

	return true;
}

Qt::ItemFlags DataSetTableModel::flags(const QModelIndex &index) const
{
	return Qt::ItemIsSelectable | Qt::ItemIsEnabled;
}

bool DataSetTableModel::setColumnType(int columnIndex, Column::ColumnType newColumnType)
{
	if (_dataSet == NULL)
		return true;

	bool changed = _dataSet->column(columnIndex).changeColumnType(newColumnType);
	emit headerDataChanged(Qt::Horizontal, columnIndex, columnIndex);

	return changed;
}

Column::ColumnType DataSetTableModel::getColumnType(int columnIndex)
{
	return _dataSet->column(columnIndex).columnType();
}

void DataSetTableModel::refreshColumn(Column * column)
{
	for(int col=0; col<_dataSet->columns().size(); col++)
		if(&(_dataSet->columns()[col]) == column)
			emit dataChanged(index(0, col), index(rowCount()-1, col));
}
