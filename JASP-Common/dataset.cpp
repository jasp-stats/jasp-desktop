
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

int DataSet::rowCount()
{
	return _rowCount;
}

int DataSet::columnCount()
{
	return _columnCount;
}

