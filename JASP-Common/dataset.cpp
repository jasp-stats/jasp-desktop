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

#include "dataset.h"
#include "log.h"

using namespace std;
/* DataSet is implemented as a set of columns */


void DataSet::setRowCount(size_t newRowCount)
{
	if(newRowCount != minRowCount() || newRowCount != maxRowCount())
	{
		_columns.setRowCount(newRowCount);

		_filterVector.clear();
		for(size_t i=0; i<newRowCount; i++)
			_filterVector.push_back(true);
	}
}

void DataSet::setColumnCount(size_t newColumnCount)
{
	if (newColumnCount != columnCount())
	{
		_columns.setColumnCount(newColumnCount);
		_columns.setRowCount(maxRowCount());
	}
}

void DataSet::setSharedMemory(boost::interprocess::managed_shared_memory *mem)
{
	_mem = mem;
	_columns.setSharedMemory(mem);

	_filterVector = BoolVector(mem->get_segment_manager());
	for(size_t i=0; i<maxRowCount(); i++)
		_filterVector.push_back(true);
}


string DataSet::toString()
{
	stringstream ss;
	ss << "Column count: " << columnCount() << "\nRow count:    " << minRowCount() << " -> " << maxRowCount() << std::endl;

	for (auto & col : _columns)
	{
		ss << "Column name: " << col.name() << "  " << col.labels().size() << "Labels" << std::endl;

		for (auto & label : col.labels())
			ss << "    "  << ", Label Text: " << label.text() << " Label Value : " << label.value() << std::endl;

		ss << "  Ints" << std::endl;
		for (int key : col.AsInts)
			ss << "    " << key << ": " << col._getLabelFromKey(key) << std::endl;

	}

	return ss.str();
}

vector<string> DataSet::resetEmptyValues(map<string, map<int, string> > emptyValuesPerColumnMap)
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

bool DataSet::setFilterVector(std::vector<bool> filterResult)
{
	bool changed = false;

	_filteredRowCount = 0;

	for(size_t i=0; i<filterResult.size(); i++)
	{
		if(_filterVector[i] != filterResult[i])
			changed = true;

		if((_filterVector[i] = filterResult[i])) //economy
			_filteredRowCount++;
	}

	return changed;
}

bool DataSet::allColumnsPassFilter() const
{
	for(const Column & col : _columns)
		if(!col.allLabelsPassFilter())
			return false;
	return true;
}

size_t DataSet::rowCount()	const
{
	size_t	min = minRowCount(),
			max = maxRowCount();

	if(min != max)
		Log::log() << "DataSet::rowCount() found inconsistent rowCounts min=" << min << " max=" << max << std::endl;

	return min;
}


void DataSet::setSynchingData(bool newVal)
{
	Log::log() << "dataset synching ? " << (newVal ? "yes" : "no") << std::endl;

	_synchingData = newVal;
}
