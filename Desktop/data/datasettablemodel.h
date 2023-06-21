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

#include "datasettableproxy.h"


///
/// Makes sure that the data from DataSetPackage is properly filtered (and possible sorted) and then passed on as a normal table-model to QML
class DataSetTableModel : public DataSetTableProxy
{
	Q_OBJECT
	Q_PROPERTY(int	columnsFilteredCount	READ columnsFilteredCount							NOTIFY columnsFilteredCountChanged)
	Q_PROPERTY(bool showInactive			READ showInactive			WRITE setShowInactive	NOTIFY showInactiveChanged)

public:
	explicit				DataSetTableModel(bool showInactive = true);
	bool					filterAcceptsRow(int source_row, const QModelIndex & source_parent)	const override;

				int			columnsFilteredCount()					const				{ return DataSetPackage::pkg()->columnsFilteredCount();								}
	Q_INVOKABLE bool		isColumnNameFree(QString name)								{ return DataSetPackage::pkg()->isColumnNameFree(name);								}
	Q_INVOKABLE QString		columnName(int column)					const;
	Q_INVOKABLE void		setColumnName(int col, QString name);
	Q_INVOKABLE QVariant	getColumnTypesWithIcons()				const				{ return DataSetPackage::pkg()->getColumnTypesWithIcons();							}
	Q_INVOKABLE bool		columnUsedInEasyFilter(int column)		const;
	Q_INVOKABLE void		resetAllFilters()											{		 DataSetPackage::pkg()->resetAllFilters();									}
	Q_INVOKABLE int			setColumnTypeFromQML(int columnIndex, int newColumnType);
	Q_INVOKABLE void		resizeData(int row, int col)								{		 DataSetPackage::pkg()->resizeData(row, col);								}

	//the following column-int passthroughs will fail once columnfiltering is added...

	int						getColumnIndex(const std::string& col)	const				{ return DataSetPackage::pkg()->getColumnIndex(col);								}

	bool					synchingData()							const				{ return DataSetPackage::pkg()->synchingData();										}

	void					pasteSpreadsheet(size_t row, size_t col, const std::vector<std::vector<QString>> & cells, QStringList newColNames = QStringList());

	bool					showInactive()							const				{ return _showInactive;	}

	QString					insertColumnSpecial(int column, bool computed, bool R);

signals:
	void					columnsFilteredCountChanged();
	void					showInactiveChanged(bool showInactive);
	void					columnTypeChanged(QString colName);
	void					labelChanged(QString columnName, QString originalLabel, QString newLabel);
	void					labelsReordered(QString columnName);

	void					renameColumnDialog(int columnIndex);

public slots:
	void					setShowInactive(bool showInactive);
				//void		onDataChanged(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles) { if( roles.count(int(DataSetPackage::specialRoles::filter)) > 0) invalidateFilter(); }


private:
	bool					_showInactive;

};

#endif // DATASETTABLEMODEL_H
