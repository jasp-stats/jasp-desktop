//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef COLUMNS_H
#define COLUMNS_H

#include "column.h"

#include <boost/interprocess/managed_shared_memory.hpp>

#include <boost/iterator/iterator_facade.hpp>

#include <boost/container/string.hpp>
#include <boost/container/vector.hpp>

typedef boost::interprocess::allocator<Column, boost::interprocess::managed_shared_memory::segment_manager> ColumnAllocator;
typedef boost::container::vector<Column, ColumnAllocator> ColumnVector;

class Columns
{
	friend class DataSet;

public:

	Columns(boost::interprocess::managed_shared_memory *mem);

	Column& at(int index);
	Column& get(std::string name);
	void removeColumn(int index);

	ColumnVector _columnStore;

	typedef ColumnVector::iterator iterator;

	iterator begin();
	iterator end();

private:

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

	boost::interprocess::managed_shared_memory *_mem;

	void setRowCount(int rowCount);
	void setColumnCount(int columnCount);

};

namespace boost
{
	template <>
	struct range_const_iterator< Columns >
	{
		typedef Columns::iterator type;
	};
}


#endif // COLUMNS_H
