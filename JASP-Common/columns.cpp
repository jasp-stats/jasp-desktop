#include "columns.h"

#include <boost/foreach.hpp>

using namespace std;
using boost::interprocess::offset_ptr;

Columns::Columns(boost::interprocess::managed_shared_memory *mem) :
	_columnStore(mem->get_segment_manager())
{
	_mem = mem;
}

Columns::~Columns()
{
}

Column *Columns::get(string name)
{	
	BOOST_FOREACH(Column &column, *this)
	{
		if (column.name() == name)
			return &column;
	}

    //qDebug() << "Columns::get(string name), name not found";

	return NULL;
}

Column &Columns::operator [](int index)
{
	int row = 0;

	BOOST_FOREACH (Column &column, _columnStore)
	{
		if (row == index)
			return column;
		row++;
	}
}

void Columns::setRowCount(int rowCount)
{
	BOOST_FOREACH(Column &column, *this)
	{
		column.setRowCount(rowCount);
	}
}

void Columns::setColumnCount(int columnCount)
{
	for (int i = _columnStore.size(); i < columnCount; i++)
	{
		Column column(_mem);
		_columnStore.push_back(column);
	}

}

Column *Columns::get(int index)
{
	int row = 0;

	BOOST_FOREACH(Column &column, _columnStore)
	{
		if (row == index)
			return &column;
		row++;
	}

    //qDebug() << "Columns::get(int), index : " << index << "not found";

	return NULL;
}

Columns::iterator Columns::begin()
{
	return _columnStore.begin();
}

Columns::iterator Columns::end()
{
	return _columnStore.end();
}

/*Columns::iterator Columns::begin()
{
	return iterator(_columnStore.begin());
}

Columns::iterator Columns::end()
{
	return iterator(_columnStore.end());
}

Columns::iterator::iterator(ColumnList::iterator parentItr)
{
	_parentItr = parentItr;
}

void Columns::iterator::increment()
{
	_parentItr++;
}

bool Columns::iterator::equal(const Columns::iterator &other) const
{
	return _parentItr == other._parentItr;
}

Column &Columns::iterator::dereference() const
{
	return *_parentItr;
}*/
