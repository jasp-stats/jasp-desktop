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
#include "datasetpackage.h"


class DataSetTableModel : public QAbstractTableModel
{
    Q_OBJECT
	Q_PROPERTY(int columnsFilteredCount READ columnsFilteredCount NOTIFY columnsFilteredCountChanged)

public:
	enum class	specialRoles { active = Qt::UserRole, lines, maxColString, columnIsComputed, computedColumnIsInvalidated, columnIsFiltered, computedColumnError };

	explicit						DataSetTableModel(QObject *parent = 0);

	QHash<int, QByteArray>			roleNames() const override;

				int					rowCount(const QModelIndex &parent = QModelIndex())									const	override;
				int					columnCount(const QModelIndex &parent = QModelIndex())								const	override;
				QVariant			data(const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
				QVariant			headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole )	const	override;
				bool				setData(const QModelIndex &index, const QVariant &value, int role)							override;
				Qt::ItemFlags		flags(const QModelIndex &index)														const	override;

	Q_INVOKABLE bool				isColumnNameFree(QString name)						{ return _package->isColumnNameFree(name.toStdString()); }
	Q_INVOKABLE bool				getRowFilter(int row)					const		{ return (row >=0 && row < rowCount()) ? _dataSet->filterVector()[row] : true; }
	Q_INVOKABLE	QVariant			columnTitle(int column)					const;
	Q_INVOKABLE QVariant			columnIcon(int column)					const;
	Q_INVOKABLE QVariant			getColumnTypesWithCorrespondingIcon()	const;
	Q_INVOKABLE bool				columnHasFilter(int column)				const;
	Q_INVOKABLE bool				columnUsedInEasyFilter(int column)		const;
	Q_INVOKABLE void				resetAllFilters();
	Q_INVOKABLE int					setColumnTypeFromQML(int columnIndex, int newColumnType);

				void				setDataSetPackage(DataSetPackage *package);
				void				clearDataSet() { setDataSetPackage(NULL); }
				size_t				addColumnToDataSet();
				int					columnsFilteredCount();
				int					getMaximumColumnWidthInCharacters(size_t columnIndex) const;
				void				setColumnsUsedInEasyFilter(std::set<std::string> usedColumns);

				bool				setColumnType(int columnIndex, Column::ColumnType newColumnType);
				Column::ColumnType	getColumnType(int columnIndex);
				bool				isComputedColumn(int colIndex)				const { return _package->isColumnComputed(colIndex); }
				bool				isComputedColumnInvalided(int colIndex)		const { return _package->isColumnInvalidated(colIndex); }
				QString				getComputedColumnError(int colIndex)		const { return QString::fromStdString(_package->getComputedColumnError(colIndex)); }

signals:
				void				columnsFilteredCountChanged();
				void				badDataEntered(const QModelIndex index);
				void				allFiltersReset();
				void				dataSetChanged(DataSet * newDataSet);
				void				columnDataTypeChanged(std::string columnName);

public slots:
				void				refresh() { beginResetModel(); endResetModel(); }
				void				refreshColumn(Column * column);
				void				columnWasOverwritten(std::string columnName, std::string possibleError);
				void				notifyColumnFilterStatusChanged(int columnIndex);
    
private:
	DataSet						*_dataSet;
	DataSetPackage				*_package;
	std::map<std::string, bool> columnNameUsedInEasyFilter;
};

#endif // DATASETTABLEMODEL_H
