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

	ColumnVector _columnStore;

	typedef ColumnVector::iterator iterator;

	iterator begin();
	iterator end();

private:

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
