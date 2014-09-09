#include "columns.h"

#include <boost/foreach.hpp>

#include "sharedmemory.h"

using namespace std;
using boost::interprocess::offset_ptr;

Columns::Columns(boost::interprocess::managed_shared_memory *mem) :
	_columnStore(mem->get_segment_manager())
{
	_mem = mem;
}

Column &Columns::get(string name)
{	
	BOOST_FOREACH(Column &column, *this)
	{
		if (column.name() == name)
			return column;
	}

	throw exception();
}

void Columns::setRowCount(int rowCount)
{
	BOOST_FOREACH(Column &column, *this)
		column.setRowCount(rowCount);
}

void Columns::setColumnCount(int columnCount)
{
	for (int i = _columnStore.size(); i < columnCount; i++)
		_columnStore.push_back(Column(_mem));
}

Column &Columns::at(int index)
{
	return _columnStore.at(index);
}

Columns::iterator Columns::begin()
{
	return _columnStore.begin();
}

Columns::iterator Columns::end()
{
	return _columnStore.end();
}

void Columns::setSharedMemory(boost::interprocess::managed_shared_memory *mem)
{
	_mem = mem;

	BOOST_FOREACH(Column &column, *this)
		column.setSharedMemory(mem);
}

