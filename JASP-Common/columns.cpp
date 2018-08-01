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

#include "columns.h"



#include "sharedmemory.h"

using namespace std;
using boost::interprocess::offset_ptr;

size_t Columns::minRowCount() const
{
	if(columnCount() == 0) return 0; //If no columns then rowcount => 0
	
	size_t minRowCount = SIZE_MAX; 

	for(const Column &column : *this)
		minRowCount = std::min(minRowCount, column.rowCount());

	return minRowCount;
}


size_t Columns::maxRowCount() const
{
	if(columnCount() == 0) return 0; //If no columns then rowcount => 0
	
	size_t maxRowCount = 0;

	for(const Column &column : *this)
		maxRowCount = std::max(maxRowCount, column.rowCount());

	return maxRowCount;
}

void Columns::setRowCount(size_t rowCount)
{
	for(Column &column : *this)
		column._setRowCount(rowCount);
}

void Columns::setColumnCount(size_t columnCount)
{
	_columnStore.reserve(columnCount);
	for (size_t i = _columnStore.size(); i < columnCount; i++)
		_columnStore.push_back(Column(_mem));
}


void Columns::removeColumn(size_t index)
{
	for (ColumnVector::iterator it = _columnStore.begin(); it != _columnStore.end(); ++it, --index)
		if (index == 0)
		{
			_columnStore.erase(it);
			return;
		}
}

void Columns::removeColumn(std::string name)
{
	for (ColumnVector::iterator it = _columnStore.begin(); it != _columnStore.end(); ++it)
		if((*it).name() == name)
		{
			_columnStore.erase(it);
			return;
		}
}


void Columns::setSharedMemory(boost::interprocess::managed_shared_memory *mem)
{
	_mem = mem;

	for(Column &column : *this)
		column.setSharedMemory(mem);
}

size_t Columns::findIndexByName(std::string name) const
{
	for(size_t i=0; i<_columnStore.size(); i++)
		if(_columnStore[i].name() == name)
			return i;

	throw std::runtime_error("Cannot find column by name: " + name);
}


Column * Columns::createColumn(std::string name)
{
	Column * column = &at(columnCount() - 1);

	column->setName(name);
	column->_setRowCount(maxRowCount());

	return column;
}

