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

#include "dataset.h"

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

Column &DataSet::column(string name)
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
	if (columnCount != _columnCount)
	{
		_columns.setColumnCount(columnCount);
		_columnCount = columnCount;
	}
}

void DataSet::removeColumn(string name)
{
	int index = getColumnIndex(name);
	_columns.removeColumn(index);
	_columnCount--;
}

void DataSet::setSharedMemory(boost::interprocess::managed_shared_memory *mem)
{
	_columns.setSharedMemory(mem);
}

int DataSet::rowCount() const
{
	return _rowCount;
}

int DataSet::columnCount() const
{
	return _columnCount;
}

int DataSet::getColumnIndex(string name)
{
	int i = 0;
	for (Columns::iterator colIt = _columns.begin(); colIt != _columns.end(); ++colIt, ++i)
	{
		if (colIt->name() == name)
			break;
	}
	return i == _columnCount ? -1 : i;
}

string DataSet::toString()
{
	stringstream ss;
	ss << "Column count: " << _columnCount << std::endl;
	ss << "Row count: " << _rowCount << std::endl;

	for (int colNr = 0; colNr < _columnCount; ++colNr)
	{
		Column& col = _columns.at(colNr);
		ss << "Column name: " << col.name() << std::endl;
		Labels& labels = col.labels();
		ss << "  " << labels.size() << "Labels" << std::endl;
		for (LabelVector::const_iterator label_it = labels.begin(); label_it != labels.end(); ++label_it)
		{
			const Label& label = *label_it;
			ss << "    "  << ", Label Text: " << label.text() << " Label Value : " << label.value() << std::endl;
		}
		ss << "  Ints" << std::endl;
		for (Column::Ints::iterator int_it = col.AsInts.begin(); int_it != col.AsInts.end(); ++int_it)
		{
			int key = *int_it;
			ss << "    " << key << ": " << col._getLabelFromKey(key) << std::endl;
		}
	}

	return ss.str();
}

vector<string> DataSet::resetEmptyValues(map<string, map<int, string> > &emptyValuesPerColumnMap)
{
	vector<string> colChanged;
	for (Columns::iterator col_it = _columns.begin(); col_it != _columns.end(); ++col_it)
	{
		Column& col = *col_it;
		map<string, map<int, string> >::iterator it = emptyValuesPerColumnMap.find(col.name());
		map<int, string> emptyValuesMap;
		if (it != emptyValuesPerColumnMap.end())
			emptyValuesMap = it->second;

		if (col.resetEmptyValues(emptyValuesMap))
			colChanged.push_back(col.name());
		emptyValuesPerColumnMap[col.name()] = emptyValuesMap;
	}

	return colChanged;
}
