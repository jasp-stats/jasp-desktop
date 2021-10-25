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

#ifndef COLUMNS_H
#define COLUMNS_H

#include "column.h"

#include <boost/interprocess/managed_shared_memory.hpp>

#include <boost/iterator/iterator_facade.hpp>

#include <boost/container/string.hpp>
#include <boost/container/vector.hpp>

struct columnNotFound : public std::runtime_error
{
	columnNotFound(std::string columnName) : std::runtime_error("Column " + columnName + " not found!") {}
	const char* what() const noexcept override;
};

typedef boost::interprocess::allocator<Column, boost::interprocess::managed_shared_memory::segment_manager> ColumnAllocator;
typedef boost::container::vector<Column, ColumnAllocator> ColumnVector;

///
/// Container class for Column, used in DataSet
///
class Columns
{
	friend class DataSet;
	friend class ComputedColumns;

public:

	Columns(boost::interprocess::managed_shared_memory *mem) : _columnStore(mem->get_segment_manager()), _mem(mem) { }

			size_t	findIndexByName(std::string name) const;
			Column& at(size_t index)							{ return _columnStore.at(index); }
	const	Column& at(size_t index)					const	{ return _columnStore.at(index); }
			Column& get(std::string name)						{ return _columnStore[findIndexByName(name)]; }
	const	Column& get(std::string name)				const	{ return _columnStore[findIndexByName(name)]; }

	void removeColumn(size_t index);
	void removeColumn(std::string name);

	ColumnVector _columnStore;

	typedef ColumnVector::iterator iterator;
	typedef ColumnVector::const_iterator const_iterator;

	iterator begin()				{ return _columnStore.begin();	}
	iterator end()					{ return _columnStore.end();	}
	const_iterator begin()	const	{ return _columnStore.begin();	}
	const_iterator end()	const	{ return _columnStore.end();	}
	size_t columnCount()	const	{ return _columnStore.size();	}
	size_t minRowCount()	const; ///< minRowCount() returns the smallest rowcount of all the columns (because they do not necessarily have the same length. (but they should)
	size_t maxRowCount()	const; ///< maxRowCount() returns the largest rowcount of all the columns (because they do not necessarily have the same length. (but they should)

			Column & operator[](size_t i)				{ return _columnStore[i]; }
	const	Column & operator[](size_t i) const			{ return _columnStore[i]; }
			Column & operator[](std::string name)		{ return _columnStore[findIndexByName(name)]; }
	const	Column & operator[](std::string name) const { return _columnStore[findIndexByName(name)]; }


	Column * initializeColumnAs(int colIndex, std::string name);
	Column * createColumn(std::string name);
	std::vector<std::string> getColumnNames();

private:
	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

	boost::interprocess::managed_shared_memory *_mem;


	void setRowCount(size_t rowCount);
	void setColumnCount(size_t columnCount);

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
