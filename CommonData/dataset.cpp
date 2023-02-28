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

		_filteredRowCount = newRowCount;
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

std::map<string, std::map<int, string> > DataSet::resetEmptyValues(const std::map<std::string, std::map<int, std::string> >& emptyValuesPerColumnMap)
{
	std::map<string, std::map<int, string> > colChanged;

	for (Column& col : _columns)
	{
		std::map<int, string> emptyValuesMap;

		if (emptyValuesPerColumnMap.count(col.name()))
			emptyValuesMap = emptyValuesPerColumnMap.at(col.name());

		if (col.resetEmptyValues(emptyValuesMap))
			colChanged[col.name()] = emptyValuesMap;
	}

	return colChanged;
}

bool DataSet::setFilterVector(std::vector<bool> filterResult)
{
	bool changed = false;


	for(size_t i=0; i<filterResult.size(); i++)
	{
		if(_filterVector[i] != filterResult[i])
			changed = true;

		_filterVector[i] = filterResult[i];
	}

	_filteredRowCount = 0;

	for(bool row : _filterVector)
		if(row)
			_filteredRowCount++;

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

size_t DataSet::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	if(columnIndex >= columnCount()) return 0;

	const Column & col = column(columnIndex);

	int extraPad = 2;

	switch(col.getColumnType())
	{
	case columnType::scale:
		return 9 + extraPad; //default precision of stringstream is 6 (and sstream is used in displaying scale values) + 3 because Im seeing some weird stuff with exp-notation  etc + some padding because of dots and whatnot

	case columnType::unknown:
		return 0;

	default:
	{
		int tempVal = 0;

		for(int labelIndex=0; labelIndex < static_cast<int>(col.labels().size()); labelIndex++)
			tempVal = std::max(tempVal, static_cast<int>(col.labels().getLabelFromRow(labelIndex).length()));

		return tempVal + extraPad;
	}
	}

}
