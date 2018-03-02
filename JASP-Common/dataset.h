//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef DATASET_H
#define DATASET_H

#include <map>
#include <iostream>
#include "columns.h"

typedef boost::interprocess::allocator<bool, boost::interprocess::managed_shared_memory::segment_manager> BoolAllocator;
typedef boost::container::vector<bool, BoolAllocator> BoolVector;

class DataSet
{
public:

	DataSet(boost::interprocess::managed_shared_memory *mem);
	~DataSet();

	int rowCount() const;
	int columnCount() const;

	Columns& columns();
	Column& column(int index);
	Column& column(std::string name);

	int getColumnIndex(std::string name);
	void setRowCount(int rowCount);
	void setColumnCount(int columnCount);
	void removeColumn(std::string name);

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

	std::string toString();
	std::vector<std::string> resetEmptyValues(std::map<std::string, std::map<int, std::string> > &emptyValuesMap);

	void setFilterVector(std::vector<bool> filterResult);
	const BoolVector& filterVector() const { return _filterVector; }
	int filteredRowCount() const { return _filteredRowCount; }

private:
	Columns		_columns;
	int			_rowCount,
				_columnCount,
				_filteredRowCount;
	BoolVector	_filterVector;

	boost::interprocess::managed_shared_memory *_mem;
};

#endif // DATASET_H
