#ifndef COLUMNS_H
#define COLUMNS_H

#include "column.h"

#include <boost/interprocess/segment_manager.hpp>
#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/offset_ptr.hpp>

#include <boost/iterator/iterator_facade.hpp>

#include <boost/container/list.hpp>
#include <boost/container/string.hpp>

#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/allocators/private_node_allocator.hpp>

typedef boost::interprocess::allocator<Column, boost::interprocess::managed_shared_memory::segment_manager> ColumnAllocator;
typedef boost::container::list<Column, ColumnAllocator> ColumnList;

class Columns
{
	friend class DataSet;

public:

	/*class iterator : public boost::iterator_facade<
			iterator, Column, boost::forward_traversal_tag>
	{

		friend class boost::iterator_core_access;

	public:

		explicit iterator(ColumnList::iterator parentItr);

	private:

		void increment();
		bool equal(iterator const& other) const;
		Column &dereference() const;

		ColumnList::iterator _parentItr;
	};*/

	typedef ColumnList::iterator iterator;

	iterator begin();
	iterator end();

	Columns();
	~Columns();

	Column *get(int index);
	Column *get(std::string name);

	Column &operator[](int index);

	ColumnList _columnStore;

private:

	void setRowCount(int rowCount);
	void setColumnCount(int columnCount);

};

namespace boost
{
	// specialize range_mutable_iterator and range_const_iterator in namespace boost
	/*template<>
	struct range_mutable_iterator< Columns >
	{
		typedef Column::AsInt::iterator type;
	};*/

	template <>
	struct range_const_iterator< Columns >
	{
		typedef Columns::iterator type;
	};
}


#endif // COLUMNS_H
