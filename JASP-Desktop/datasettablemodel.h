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

public:
    explicit DataSetTableModel(QObject *parent = 0);

    void setDataSet(DataSet *dataSet);
	void clearDataSet();

    virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
    virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
    virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
    virtual QVariant headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole ) const OVERRIDE;
    virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;
    virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

	void setColumnType(int columnIndex, Column::ColumnType newColumnType);
	Column::ColumnType getColumnType(int columnIndex);
    
signals:

	void badDataEntered(const QModelIndex index);
    
private:
	DataSet *_dataSet;

	QIcon _nominalTextIcon;
	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;
};

#endif // DATASETTABLEMODEL_H
