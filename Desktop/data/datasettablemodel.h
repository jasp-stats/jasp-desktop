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
	static DataSetTableModel* singleton()	{ return _singleton; }

	explicit				DataSetTableModel();
	~DataSetTableModel()	override { if(_singleton == this) _singleton = nullptr; }

	bool					filterAcceptsRow(int source_row, const QModelIndex & source_parent)	const override;

				int			columnsFilteredCount()					const				{ return DataSetPackage::pkg()->columnsFilteredCount();								}
	Q_INVOKABLE bool		isColumnNameFree(QString name)								{ return DataSetPackage::pkg()->isColumnNameFree(name);								}
	Q_INVOKABLE	QVariant	columnTitle(int column)					const				{ return DataSetPackage::pkg()->getColumnTitle(column);								}
	Q_INVOKABLE QVariant	columnIcon(int column)					const				{ return DataSetPackage::pkg()->getColumnIcon(column);								}
	Q_INVOKABLE QVariant	getColumnTypesWithCorrespondingIcon()	const				{ return DataSetPackage::pkg()->getColumnTypesWithCorrespondingIcon();				}
	Q_INVOKABLE bool		columnHasFilter(int column)				const				{ return DataSetPackage::pkg()->getColumnHasFilter(column);							}
	Q_INVOKABLE bool		columnUsedInEasyFilter(int column)		const				{ return DataSetPackage::pkg()->isColumnUsedInEasyFilter(column);					}
	Q_INVOKABLE void		resetAllFilters()											{		 DataSetPackage::pkg()->resetAllFilters();									}
	Q_INVOKABLE int			setColumnTypeFromQML(int columnIndex, int newColumnType)	{ return DataSetPackage::pkg()->setColumnTypeFromQML(columnIndex, newColumnType);	}

	columnType				getColumnType(size_t column)			const				{ return DataSetPackage::pkg()->getColumnType(column);								}
	std::string				getColumnName(size_t col)				const				{ return DataSetPackage::pkg()->getColumnName(col);									}
	int						getColumnIndex(const std::string& col)	const				{ return DataSetPackage::pkg()->getColumnIndex(col);								}
	QStringList				getColumnLabelsAsStringList(int col)	const;
	size_t					getMaximumColumnWidthInCharacters(int index) const			{ return DataSetPackage::pkg()->getMaximumColumnWidthInCharacters(index);			}
	QModelIndex				parentModelForType(parIdxType type, int column = 0)	const	{ return DataSetPackage::pkg()->parentModelForType(type, column);					}
	bool					synchingData()							const				{ return DataSetPackage::pkg()->synchingData();										}

				bool		showInactive()							const				{ return _showInactive;	}

signals:
				void		columnsFilteredCountChanged();
				void		showInactiveChanged(bool showInactive);
				void		columnTypeChanged(QString colName);
				void		labelChanged(QString columnName, QString originalLabel, QString newLabel);
				void		labelsReordered(QString columnName);

public slots:
				void		setShowInactive(bool showInactive);
				//void		onDataChanged(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles) { if( roles.count(int(DataSetPackage::specialRoles::filter)) > 0) invalidateFilter(); }


private:
	bool					_showInactive	= true;

	static DataSetTableModel* _singleton;

};

#endif // DATASETTABLEMODEL_H
