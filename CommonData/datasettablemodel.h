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

#include <QSortFilterProxyModel>
#include "dataset.h"
#include "qutils.h"

///
/// Makes sure that the data from DataSetPackage is properly filtered (and possible sorted) and then passed on as a normal table-model to QML
class DataSetTableModel : public QSortFilterProxyModel
{
	Q_OBJECT
	Q_PROPERTY(int			columnsLabelFilteredCount	READ columnsLabelFilteredCount							NOTIFY columnsLabelFilteredCountChanged) ///< Columns with filters selected *in* them
	Q_PROPERTY(bool			showInactive			READ showInactive			WRITE setShowInactive	NOTIFY showInactiveChanged)
	Q_PROPERTY(QString		columnFilter			READ columnFilter			WRITE setColumnFilter	NOTIFY columnFilterChanged) ///< The filter used on columnnames for display
	Q_PROPERTY(QStringList	currentTypeIcons		READ currentTypeIcons								NOTIFY currentTypeIconsChanged		)
	
public:
	explicit				DataSetTableModel(QObject * parent, bool showInactive = true);
	bool					filterAcceptsRow(	int source_row,		const QModelIndex & source_parent)	const override;
	bool					filterAcceptsColumn(int source_column,	const QModelIndex &source_parent)	const override;
	
				int			columnsLabelFilteredCount()					const				{ return dataSetSourceModel()->columnsLabelFilteredCount();								}
	Q_INVOKABLE bool		isColumnNameFree(QString name)								{ return dataSetSourceModel()->isColumnNameFree(fq(name));								}
				QString		columnFilter() const;

	Q_INVOKABLE void		setColumnName(int col, QString name);
	Q_INVOKABLE QVariant	columnTypesWithIcons()				const				{ return getColumnTypesWithIcons();							}
				void		setColumnFilter(const QString &newColumnFilter);
	Q_INVOKABLE bool		columnUsedInEasyFilter(int column)		const;
	Q_INVOKABLE void		resetAllFilters()											{		 dataSetSourceModel()->resetAllFilters();									}
				
				DataSet *	dataSetSourceModel() const { return qobject_cast<DataSet*>(sourceModel()); }
	
	//the following column-int passthroughs will fail once columnfiltering is added...

	int						getColumnIndex(const std::string& col)	const				{ return dataSetSourceModel()->getColumnIndex(col);								}
	bool					synchingData()							const				{ return dataSetSourceModel()->synchingData();										}
	void					pasteSpreadsheet(size_t row, size_t col, const std::vector<std::vector<QString>> & values, const std::vector<std::vector<QString>> & labels, const std::vector<int> & colTypes = std::vector<int>(), const QStringList & colNames = {}, const std::vector<boolvec> & selected = {});
	bool					showInactive()							const				{ return _showInactive;	}

	QString					insertColumnSpecial(int column, const QMap<QString, QVariant>& props);
	
	QStringList				currentTypeIcons() const;
	Q_INVOKABLE	void		toggleColType(int i, bool doubleClick);	

signals:
	void					columnsLabelFilteredCountChanged();
	void					showInactiveChanged(bool showInactive);
	void					labelChanged(const Column * column, QString originalLabel, QString newLabel);
	void					labelsReordered(QString columnName);
	void					columnTypeChanged(QString name);
	void					emptyValuesChanged();
	void					columnFilterChanged(QString);
	void					renameColumnDialog(int columnIndex);
	void					currentTypeIconsChanged();

public slots:
	QString					columnName(int column)					const;
	void					setShowInactive(bool showInactive);
	void					handleDataSetChange(DataSet * dataSet);
				//void		onDataChanged(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles) { if( roles.count(int(dataPkgRoles::filter)) > 0) invalidateFilter(); }

private slots:
	void					onSourceModelChanged();

private:
	bool					_showInactive;
	QString					_columnFilter;
	std::vector<bool>		_colTypesShown = {true, true, true };
	
	std::vector<QMetaObject::Connection>	_connections;
};

#endif // DATASETTABLEMODEL_H
