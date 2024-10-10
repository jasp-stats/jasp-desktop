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
#include "utilities/qutils.h"
#include "log.h"

DataSetTableModel::DataSetTableModel(bool showInactive) 
: DataSetTableProxy(DataSetPackage::pkg()->dataSubModel()), _showInactive(showInactive)
{
	
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnsFilteredCountChanged,	this, &DataSetTableModel::columnsFilteredCountChanged	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnDataTypeChanged,			this, [&](QString colName) { emit columnTypeChanged(colName, int(DataSetPackage::pkg()->getColumnType(colName)));	}, Qt::QueuedConnection);
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelChanged,					this, &DataSetTableModel::labelChanged					);
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelsReordered,				this, &DataSetTableModel::labelsReordered				);
	connect(DataSetPackage::pkg(),	&DataSetPackage::workspaceEmptyValuesChanged,	this, &DataSetTableModel::emptyValuesChanged			);
	//connect(this,		&DataSetTableModel::dataChanged,				this, &DataSetTableModel::onDataChanged,				Qt::QueuedConnection);

	setFilterRole(int(DataSetPackage::specialRoles::filter));
}


void DataSetTableModel::setShowInactive(bool showInactive)
{
	if (_showInactive == showInactive)
		return;

	_showInactive = showInactive;
	emit showInactiveChanged(_showInactive);
	invalidate();
	beginResetModel();
	endResetModel();
}

bool DataSetTableModel::filterAcceptsRow(int source_row, const QModelIndex & source_parent)	const
{
	return (_showInactive || DataSetPackage::pkg()->getRowFilter(source_row));
}

QString DataSetTableModel::columnName(int column) const
{
	int pkgColIndex = data(index(0, column), int(DataSetPackage::specialRoles::columnPkgIndex)).toInt();
	return	DataSetPackage::pkg()->data(
				DataSetPackage::pkg()->index(0, pkgColIndex, DataSetPackage::pkg()->indexForSubNode(node())),
				int(DataSetPackage::specialRoles::name)
			).toString();
}

void DataSetTableModel::setColumnName(int col, QString name)
{
	setData(index(0, col), name, int(DataSetPackage::specialRoles::name));
}

bool DataSetTableModel::columnUsedInEasyFilter(int column) const
{
	int pkgColIndex = data(index(0, column), int(DataSetPackage::specialRoles::columnPkgIndex)).toInt();
	return	DataSetPackage::pkg()->data(
				DataSetPackage::pkg()->index(0, pkgColIndex, DataSetPackage::pkg()->indexForSubNode(node())),
				int(DataSetPackage::specialRoles::inEasyFilter)
			).toBool();
}

void DataSetTableModel::pasteSpreadsheet(size_t row, size_t col, const std::vector<std::vector<QString> > & values, const std::vector<std::vector<QString>> & labels, const std::vector<int> & colTypes, const QStringList & colNames, const std::vector<boolvec> & selected)
{
	QModelIndex idx = mapToSource(index(row, col));
	DataSetPackage::pkg()->pasteSpreadsheet(idx.row() == -1 ? row : idx.row(), idx.column() == -1 ? col : idx.column(), values, labels, colTypes, colNames, selected);
}

QString DataSetTableModel::insertColumnSpecial(int column, const QMap<QString, QVariant>& props)
{
	if(column >= columnCount())
		return subNodeModel()->appendColumnSpecial(props);

	int sourceColumn = column > columnCount() ? columnCount() : column;
	sourceColumn = mapToSource(index(0, sourceColumn)).column();

	return subNodeModel()->insertColumnSpecial(sourceColumn == -1 ? sourceModel()->columnCount() : sourceColumn, props);
}



