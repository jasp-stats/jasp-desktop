#ifndef DATASET_H
#define DATASET_H

#include <vector>

#include "columns.h"
#include "datablock.h"

#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/offset_ptr.hpp>
#include <boost/iterator/iterator_facade.hpp>

#include <boost/interprocess/containers/map.hpp>
#include <boost/interprocess/containers/list.hpp>
#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/allocators/private_node_allocator.hpp>


class DataSet
{
public:
	/* columnInfo and data should be byref, but the shared memory constructor doesn't like them */
	DataSet(boost::interprocess::managed_shared_memory *mem, std::vector<std::string> *columnInfo, std::vector<std::vector<std::string> > *data);
	DataSet(boost::interprocess::managed_shared_memory *mem);

	int rowCount();
	int columnCount();

	Columns &columns();

	void setRowCount(int rowCount);
	void setColumnCount(int rowCount);

private:

	boost::interprocess::managed_shared_memory *_mem;

	Columns _columns;

	int _rowCount;
	int _columnCount;

};

#endif // DATASET_H
