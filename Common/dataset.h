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

#include "columns.h"

typedef boost::interprocess::allocator<bool, boost::interprocess::managed_shared_memory::segment_manager> BoolAllocator;
typedef boost::container::vector<bool, BoolAllocator> BoolVector;

class DataSet
{
	typedef std::map<std::string, std::map<int, std::string>> emptyValsType;

public:

	DataSet(boost::interprocess::managed_shared_memory *mem) : _columns(mem), _filterVector(mem->get_segment_manager()), _mem(mem) { }
	~DataSet() {}

	size_t minRowCount()	const	{ return _columns.minRowCount(); }
	size_t maxRowCount()	const	{ return _columns.maxRowCount(); }
	size_t rowCount()		const;
	size_t columnCount()	const	{ return _columns.columnCount(); }

	Columns& columns()				{ return _columns; }

			Column& column(size_t index)				{ return _columns.at(index);	}
			Column& column(std::string name)			{ return _columns.get(name);	}
	const	Column& column(size_t index)		const	{ return _columns.at(index);	}
	const	Column& column(std::string name)	const	{ return _columns.get(name);	}

	int  getColumnIndex(std::string name) { try{ return _columns.findIndexByName(name); } catch(...) { return -1;	} }
	void setRowCount(size_t rowCount);
	void setColumnCount(size_t columnCount);

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

	std::string toString();
	std::map<std::string, std::map<int, std::string> > resetEmptyValues(const emptyValsType& emptyValuesMap);

	bool				setFilterVector(std::vector<bool> filterResult);
	BoolVector	&		filterVector()				{ return _filterVector; }
	int					filteredRowCount()	const	{ return _filteredRowCount; }

	bool allColumnsPassFilter()				const;

	size_t						getMaximumColumnWidthInCharacters(size_t columnIndex) const;
	std::vector<std::string> 	getColumnNames() { return _columns.getColumnNames();};

private:
	Columns			_columns;
	int				_filteredRowCount = 0;
	BoolVector		_filterVector;

	boost::interprocess::managed_shared_memory *_mem;
};

#endif // DATASET_H
