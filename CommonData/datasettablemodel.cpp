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
#include "qutils.h"
#include "log.h"
#include "dataenums.h"
#include "variableinfo.h"

DataSetTableModel::DataSetTableModel(QObject * parent, bool showInactive) 
: QSortFilterProxyModel(parent), 
  _showInactive(showInactive)
{
	connect(this, &DataSetTableModel::sourceModelChanged, this, &DataSetTableModel::onSourceModelChanged);
	
	setFilterRole(int(dataPkgRoles::filter));
}

void DataSetTableModel::onSourceModelChanged()
{
	for(QMetaObject::Connection & connection : _connections)
		if(connection)
			disconnect(connection);
	_connections.clear();
	
	DataSet * dataSet = dataSetSourceModel();
	
	if(!dataSet)
		return;

	_connections.push_back(connect(dataSet,	&DataSet::columnsLabelFilteredCountChanged,	this, &DataSetTableModel::columnsLabelFilteredCountChanged	, Qt::UniqueConnection));
	_connections.push_back(connect(dataSet,	&DataSet::labelChanged,						this, &DataSetTableModel::labelChanged						, Qt::UniqueConnection));
	_connections.push_back(connect(dataSet,	&DataSet::labelsReordered,					this, &DataSetTableModel::labelsReordered					, Qt::UniqueConnection));
	_connections.push_back(connect(dataSet,	&DataSet::columnTypeChanged,				this, &DataSetTableModel::columnTypeChanged					, Qt::UniqueConnection));
	
	_connections.push_back(connect(dataSet,	&DataSet::shownFilterChanged,			this, &DataSetTableModel::invalidateFilter			));
	
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

void DataSetTableModel::handleDataSetChange(DataSet * dataSet)
{
	setSourceModel(dataSet);
	invalidate();
	beginResetModel();
	endResetModel();
}

bool DataSetTableModel::filterAcceptsRow(int source_row, const QModelIndex & source_parent)	const
{
	if(_showInactive || !dataSetSourceModel())
		return true;
	
	Filter * f = dataSetSourceModel()->shownFilter();
	
	return !f || f->filtered().size() <= source_row || f->filtered()[source_row];
}

bool DataSetTableModel::filterAcceptsColumn(int source_column, const QModelIndex &source_parent) const
{
	if(!dataSetSourceModel())
		return false;
	
	Column *	column	= dataSetSourceModel()->column(source_column);
	QString		name	= column ? column->nameQ() : "";
	bool		pass	= _columnFilter == "" || name.contains(_columnFilter);
	
	columnType colType	= column ? column->type() : columnType::unknown;
	
	switch(colType)
	{
	case columnType::scale:
	case columnType::ordinal:
	case columnType::nominal:
		if(!_colTypesShown[colType == columnType::scale ? 0 : colType == columnType::ordinal ? 1 : 2])
			pass = false;
		break;
	
	default:
		break;
	}

	return pass;
}

QString DataSetTableModel::columnName(int column) const
{
  return data(index(0, column), int(dataPkgRoles::name)).toString();
}

void DataSetTableModel::setColumnName(int col, QString name)
{
	setData(index(0, col), name, int(dataPkgRoles::name));
}

bool DataSetTableModel::columnUsedInEasyFilter(int column) const
{
  return data(index(0, column), int(dataPkgRoles::inEasyFilter)).toBool();
}

void DataSetTableModel::pasteSpreadsheet(size_t row, size_t col, const std::vector<std::vector<QString> > & values, const std::vector<std::vector<QString>> & labels, const std::vector<int> & colTypes, const QStringList & colNames, const std::vector<boolvec> & selected)
{
	QModelIndex idx = mapToSource(index(row, col));
	dataSetSourceModel()->pasteSpreadsheet(idx.row() == -1 ? row : idx.row(), idx.column() == -1 ? col : idx.column(), values, labels, colTypes, colNames, selected);
}

QString DataSetTableModel::insertColumnSpecial(int column, const QMap<QString, QVariant>& props)
{
	if(column >= columnCount())
		return dataSetSourceModel()->insertColumnSpecial(sourceModel()->columnCount(), props);

	int sourceColumn = column > columnCount() ? columnCount() : column;
	sourceColumn = mapToSource(index(0, sourceColumn)).column();

	return dataSetSourceModel()->insertColumnSpecial(sourceColumn == -1 ? sourceModel()->columnCount() : sourceColumn, props);
}

QString DataSetTableModel::columnFilter() const
{
	return _columnFilter;
}

void DataSetTableModel::setColumnFilter(const QString &newColumnFilter)
{
	if (_columnFilter == newColumnFilter)
		return;

	beginFilterChange();
	_columnFilter = newColumnFilter;
	emit columnFilterChanged(_columnFilter);
	endFilterChange(QSortFilterProxyModel::Direction::Columns);
	beginResetModel();
	endResetModel();
}


QStringList DataSetTableModel::currentTypeIcons() const
{
	const auto _columnType = [](int i){ return i == 0 ? columnType::scale : i == 1 ? columnType::ordinal : columnType::nominal; };
	QStringList list;
	for(int i=0; i<_colTypesShown.size(); i++)
		list.push_back(getIconFilename(_columnType(i), _colTypesShown[i] ? varIconType::DefaultIconType : varIconType::InactiveIconType));
	return list;
}

void DataSetTableModel::toggleColType(int i, bool doubleClick)
{
	beginFilterChange();
	
	if(!doubleClick)
	{
		if(i>=0 && i<_colTypesShown.size())
			_colTypesShown[i] = !_colTypesShown[i];
	}
	else
		for(int c=0; c<_colTypesShown.size(); c++)
			_colTypesShown[c] = c==i;
	
	emit currentTypeIconsChanged();
	
	endFilterChange(QSortFilterProxyModel::Direction::Columns);
	beginResetModel();
	endResetModel();
}
