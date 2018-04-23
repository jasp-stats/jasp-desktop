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

#ifndef DATASETTABLEMODEL_H
#define DATASETTABLEMODEL_H

#include <QModelIndex>
#include <QAbstractTableModel>
#include <QIcon>

#include "common.h"
#include "dataset.h"


class DataSetTableModel : public QAbstractTableModel
{
    Q_OBJECT
	Q_PROPERTY(int columnsFilteredCount READ columnsFilteredCount NOTIFY columnsFilteredCountChanged)

public:
    explicit DataSetTableModel(QObject *parent = 0);

    void setDataSet(DataSet *dataSet);
	void clearDataSet() { setDataSet(NULL); }

    virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
    virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	virtual QVariant headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole ) const OVERRIDE;
    virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;
    virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

	Q_INVOKABLE int setColumnTypeFromQML(int columnIndex, int newColumnType) { setColumnType(columnIndex, (Column::ColumnType)newColumnType); return getColumnType(columnIndex); }
	bool setColumnType(int columnIndex, Column::ColumnType newColumnType);
	Column::ColumnType getColumnType(int columnIndex);


	virtual QHash<int, QByteArray> roleNames() const OVERRIDE;
	Q_INVOKABLE QStringList userRoleNames() const;
	Q_INVOKABLE QVariant columnTitle(int column) const;
	Q_INVOKABLE QVariant columnIcon(int column) const;
	Q_INVOKABLE QVariant getCellValue(int column, int row) const { return data(index(row, column), Qt::DisplayRole); }
	Q_INVOKABLE QVariant getColumnTypesWithCorrespondingIcon();
	Q_INVOKABLE QVariant getRowFilter(int row) { return (row >=0 && row < rowCount()) ? _dataSet->filterVector()[row] : true; }
	Q_INVOKABLE QVariant columnHasFilter(int column) const;

	Q_INVOKABLE void resetAllFilters();

	int columnsFilteredCount();
	void notifyColumnFilterStatusChanged() { emit columnsFilteredCountChanged(); }
    
signals:
	void columnsFilteredCountChanged();
	void badDataEntered(const QModelIndex index);
	void allFiltersReset();

public slots:
	void refresh() { beginResetModel(); endResetModel(); }
	void refreshColumn(Column * column);
    
private:
	DataSet *_dataSet;

	QIcon _nominalTextIcon;
	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;
};

#endif // DATASETTABLEMODEL_H
