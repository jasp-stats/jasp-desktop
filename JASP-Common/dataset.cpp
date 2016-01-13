//
// Copyright (C) 2013-2016 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "dataset.h"

/*
 * DataSet is implemented as a set of columns
 */

DataSet::DataSet(boost::interprocess::managed_shared_memory *mem) :
	_columns(mem)
{
	_rowCount = 0;
	_columnCount = 0;
}

DataSet::~DataSet()
{
}

Columns &DataSet::columns()
{
	return _columns;
}

Column &DataSet::column(int index)
{
	return _columns.at(index);
}

Column &DataSet::column(std::string name)
{
	return _columns.get(name);
}

void DataSet::setRowCount(int rowCount)
{
	_columns.setRowCount(rowCount);
	_rowCount = rowCount;
}

void DataSet::setColumnCount(int columnCount)
{
	_columns.setColumnCount(columnCount);
	_columnCount = columnCount;
}

void DataSet::setSharedMemory(boost::interprocess::managed_shared_memory *mem)
{
	_columns.setSharedMemory(mem);
}

int DataSet::rowCount() const
{
	return _rowCount;
}

int DataSet::columnCount() const
{
	return _columnCount;
}

