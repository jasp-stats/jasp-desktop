
#include "dataset.h"


#include <QDebug>

#include <vector>
#include <string>
#include <algorithm>
#include <climits>

#include "boost/foreach.hpp"
#include "boost/lexical_cast.hpp"

#include "csvparser.h"
#include "column.h"

using namespace std;

/*
 * DataSet is implemented as a set of columns
 */

DataSet::DataSet(boost::interprocess::managed_shared_memory *mem) :
	_columns(mem)
{
	_rowCount = 0;
	_columnCount = 0;
}

Columns &DataSet::columns()
{
	return _columns;
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

int DataSet::rowCount()
{
	return _rowCount;
}

int DataSet::columnCount()
{
	return _columnCount;
}

