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

#include "utilities/qutils.h"
#include "sharedmemory.h"

using namespace std;

DataSetTableModel::DataSetTableModel(QObject *parent) :
    QAbstractTableModel(parent)
{
	_dataSet = NULL;
}

QVariant DataSetTableModel::getColumnTypesWithCorrespondingIcon() const
{
	static QVariantList ColumnTypeAndIcons;

	//enum ColumnType { ColumnTypeUnknown = 0, ColumnTypeNominal = 1, ColumnTypeNominalText = 2, ColumnTypeOrdinal = 4, ColumnTypeScale = 8 };

	if(ColumnTypeAndIcons.size() == 0)
	{
		ColumnTypeAndIcons.push_back(QVariant(QString("")));
		ColumnTypeAndIcons.push_back(QVariant(QString("../icons/variable-nominal.svg")));
		ColumnTypeAndIcons.push_back(QVariant(QString("../icons/variable-nominal-text.svg")));
		ColumnTypeAndIcons.push_back(QVariant(QString("")));
		ColumnTypeAndIcons.push_back(QVariant(QString("../icons/variable-ordinal.svg")));
		ColumnTypeAndIcons.push_back(QVariant(QString("")));
		ColumnTypeAndIcons.push_back(QVariant(QString("")));
		ColumnTypeAndIcons.push_back(QVariant(QString("")));
		ColumnTypeAndIcons.push_back(QVariant(QString("../icons/variable-scale.svg")));
	}

	return QVariant(ColumnTypeAndIcons);
}

void DataSetTableModel::setDataSetPackage(DataSetPackage *package)
{
    beginResetModel();
	_dataSet = package == NULL ? NULL : package->dataSet();
	_package = package;
    endResetModel();

	emit columnsFilteredCountChanged();
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

	int column = index.column();


	if(column > -1 && column < columnCount())
	{
		if(role == Qt::DisplayRole)
			return tq(_dataSet->column(column)[index.row()]);
		else if(role == (int)specialRoles::active)
			return getRowFilter(index.row());
		else if(role == (int)specialRoles::lines)
		{
			bool	iAmActive = getRowFilter(index.row()),
					//aboveMeIsActive = index.row() > 0				&& data(this->index(index.row() - 1, index.column()), (int)specialRoles::active).toBool();
					belowMeIsActive = index.row() < rowCount() - 1	&& data(this->index(index.row() + 1, index.column()), (int)specialRoles::active).toBool();
					//iAmLastRow = index.row() == rowCount() - 1;

			bool	up		= iAmActive,
					left	= iAmActive,
					down	= iAmActive && !belowMeIsActive,
					right	= iAmActive && index.column() == columnCount() - 1; //always draw left line and right line only if last col

			return	(left ?		1 : 0) +
					(right ?	2 : 0) +
					(up ?		4 : 0) +
					(down ?		8 : 0);
		}


	}

    return QVariant();
}

QVariant DataSetTableModel::columnTitle(int column) const
{
	if(column >= 0 && size_t(column) < _dataSet->columnCount())
	{
		QString value = tq(_dataSet->column(column).name());
		return QVariant(value);
	}
	else
		return QVariant();
}

QVariant DataSetTableModel::columnIcon(int column) const
{
	if(column >= 0 && size_t(column) < _dataSet->columnCount())
	{
		Column &columnref = _dataSet->column(column);
		return QVariant(columnref.columnType());
	}
	else
		return QVariant(-1);
}

bool DataSetTableModel::columnHasFilter(int column) const
{
	if(_dataSet != NULL && column >= 0 && size_t(column) < _dataSet->columnCount())
		return _dataSet->column(column).hasFilter();
	return false;
}

bool DataSetTableModel::columnUsedInEasyFilter(int column) const
{
	if(_dataSet != NULL && size_t(column) < _dataSet->columnCount())
	{
		std::string colName = _dataSet->column(column).name();
		return columnNameUsedInEasyFilter.count(colName) > 0 && columnNameUsedInEasyFilter.at(colName);
	}
	return false;
}

int DataSetTableModel::columnsFilteredCount()
{
	if(_dataSet == NULL) return 0;

	int colsFiltered = 0;

	for(auto & col : _dataSet->columns())
		if(col.hasFilter())
			colsFiltered++;

	return colsFiltered;
}

void DataSetTableModel::resetAllFilters()
{
	for(auto & col : _dataSet->columns())
		col.resetFilter();

	emit allFiltersReset();
	emit columnsFilteredCountChanged();
	emit headerDataChanged(Qt::Horizontal, 0, columnCount());
}

int DataSetTableModel::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	if(columnIndex >= _dataSet->columnCount()) return 0;

	Column & col = _dataSet->column(columnIndex);

	int extraPad = 2;

	switch(col.columnType())
	{
	case Column::ColumnTypeScale:
		return 6 + extraPad; //default precision of stringstream is 6 (and sstream is used in displaying scale values) + some padding because of dots and whatnot

	case Column::ColumnTypeUnknown:
		return 0;

	default:
	{
		int tempVal = 0;

		for(size_t labelIndex=0; labelIndex < col.labels().size(); labelIndex++)
			tempVal = std::max(tempVal, (int)col.labels().getLabelFromRow(labelIndex).length());

		return tempVal + extraPad;
	}
	}

}

QVariant DataSetTableModel::headerData ( int section, Qt::Orientation orientation, int role) const
{
	if (_dataSet == NULL)
		return QVariant();

	if (role == Qt::DisplayRole)
	{
		if (orientation == Qt::Horizontal)
		{
			QString value = tq(_dataSet->column(section).name());
			return QVariant(value);
		}
		else
		{
			return QVariant(section + 1);
		}
	}
	else if(role == (int)specialRoles::maxColString) //A query from DataSetView for the maximumlength string to be expected! This to accomodate columnwidth
	{
		//calculate some maximum string?
		QString dummyText = headerData(section, orientation, Qt::DisplayRole).toString() + "XXXXX" + (isComputedColumn(section) ? "XXXXX" : ""); //Bit of padding for filtersymbol and columnIcon
		int colWidth = getMaximumColumnWidthInCharacters(section);

		while(colWidth > dummyText.length())
			dummyText += "X";

		return dummyText;
	}
	else if(role == Qt::TextAlignmentRole)							return QVariant(Qt::AlignCenter);
	else if(role == (int)specialRoles::columnIsComputed)			return isComputedColumn(section);
	else if(role == (int)specialRoles::computedColumnIsInvalidated)	return isComputedColumnInvalided(section);
	else if(role == (int)specialRoles::columnIsFiltered)			return columnHasFilter(section) || columnUsedInEasyFilter(section);
	else if(role == (int)specialRoles::computedColumnError)			return getComputedColumnError(section);


	return QVariant();
}

QHash<int, QByteArray> DataSetTableModel::roleNames() const
{
	QHash<int, QByteArray> roles = QAbstractTableModel::roleNames ();


	roles[(int)specialRoles::active]						= QString("active").toUtf8();
	roles[(int)specialRoles::lines]							= QString("lines").toUtf8();
	roles[(int)specialRoles::maxColString]					= QString("maxColString").toUtf8();
	roles[(int)specialRoles::columnIsFiltered]				= QString("columnIsFiltered").toUtf8();
	roles[(int)specialRoles::columnIsComputed]				= QString("columnIsComputed").toUtf8();
	roles[(int)specialRoles::computedColumnError]			= QString("computedColumnError").toUtf8();
	roles[(int)specialRoles::computedColumnIsInvalidated]	= QString("computedColumnIsInvalidated").toUtf8();

	return roles;
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
	for(size_t col=0; col<_dataSet->columns().columnCount(); col++)
		if(&(_dataSet->columns()[col]) == column)
			emit dataChanged(index(0, col), index(rowCount()-1, col));
}

void DataSetTableModel::columnWasOverwritten(std::string columnName, std::string possibleError)
{
	for(size_t col=0; col<_dataSet->columns().columnCount(); col++)
		if(_dataSet->columns()[col].name() == columnName)
			emit dataChanged(index(0, col), index(rowCount()-1, col));
}

int DataSetTableModel::setColumnTypeFromQML(int columnIndex, int newColumnType)
{
	setColumnType(columnIndex, (Column::ColumnType)newColumnType);

	emit headerDataChanged(Qt::Orientation::Horizontal, columnIndex, columnIndex);
	emit columnDataTypeChanged(_dataSet->column(columnIndex).name());

	return getColumnType(columnIndex);
}

void DataSetTableModel::setColumnsUsedInEasyFilter(std::set<std::string> usedColumns)
{
	columnNameUsedInEasyFilter.clear();

	for(auto & col : usedColumns)
	{
		columnNameUsedInEasyFilter[col] = true;
		try { notifyColumnFilterStatusChanged(_dataSet->columns().findIndexByName(col)); } catch(...) {}
	}
}

void DataSetTableModel::notifyColumnFilterStatusChanged(int columnIndex)
{
	emit columnsFilteredCountChanged();
	emit headerDataChanged(Qt::Horizontal, columnIndex, columnIndex);
}

