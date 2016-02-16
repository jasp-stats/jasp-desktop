//
// Copyright (C) 2013-2016 University of Amsterdam
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

#include "column.h"

#include <boost/foreach.hpp>
#include <sstream>
#include <string>

#include <boost/lexical_cast.hpp>
#include <cmath>
#include <iostream>

using namespace boost::interprocess;
using namespace boost;
using namespace std;

Column::Column(managed_shared_memory *mem) :
	_name(mem->get_segment_manager()),
	_blocks(std::less<ull>(), mem->get_segment_manager()),
	_labels(mem)
{
	_mem = mem;
	_rowCount = 0;
	_columnType = Column::ColumnTypeNominal;
}

Column::~Column()
{
	BOOST_FOREACH(BlockEntry &entry, _blocks)
		_mem->destroy_ptr(&*entry.second);
}

Labels &Column::labels()
{
	return _labels;
}

Column &Column::operator=(const Column &column)
{
	if (&column != this)
	{
		this->_name = column._name;
		this->_rowCount = column._rowCount;
		this->_columnType = column._columnType;
		this->_blocks = column._blocks;
		this->_labels = column._labels;
	}

	return *this;
}

void Column::setSharedMemory(managed_shared_memory *mem)
{
	_mem = mem;
	_labels.setSharedMemory(mem);
}

Column::ColumnType Column::columnType() const
{
	return _columnType;
}

void Column::changeColumnType(Column::ColumnType newColumnType)
{
	if (newColumnType == _columnType)
		return;

	if (newColumnType == ColumnTypeOrdinal || newColumnType == ColumnTypeNominal)
	{
		if (_columnType == ColumnTypeNominal || _columnType == ColumnTypeOrdinal)
		{
			_columnType = newColumnType;
		}
		else if (_columnType == ColumnTypeNominalText)
		{
			Labels& labels = this->labels();
			labels.clear();

			map<int, int> oldIndexToNewIndex;

			for (int i = 0; i < labels.size(); i++)
			{
				try
				{
					std::string text = labels.labelFor(i).text();
					int value = lexical_cast<int>(text);
					int raw = labels.add(value);
					oldIndexToNewIndex[i] = raw;
				}
				catch (...)
				{
				}
			}

			Ints::iterator intIterator = AsInts.begin();
			for (; intIterator != AsInts.end(); intIterator++)
			{
				int value = *intIterator;
				map<int, int>::iterator itr = oldIndexToNewIndex.find(value);
				if (itr != oldIndexToNewIndex.end())
					*intIterator = itr->second;
				else
					*intIterator = INT_MIN;

			}

			_columnType = newColumnType;
		}
		else if (_columnType == ColumnTypeScale)
		{
			std::set<int> uniqueValues;

			Doubles::iterator doubles = this->AsDoubles.begin();
			for (; doubles != this->AsDoubles.end(); doubles++)
			{
				int v = (int)*doubles;
				if ( ! std::isnan(v))
					uniqueValues.insert(v);
			}

			Labels& labels = this->labels();
			map<int, int> valueToIndex;

			set<int>::iterator itr = uniqueValues.begin();
			for (; itr != uniqueValues.end(); itr++)
			{
				int v = *itr;
				int raw = labels.add(v);
				valueToIndex[v] = raw;
			}

			Ints::iterator ints = this->AsInts.begin();
			Ints::iterator end = this->AsInts.end();
			doubles = this->AsDoubles.begin();

			for (; ints != end; ints++, doubles++)
			{
				double value = *doubles;
				if (std::isnan(value))
				{
					*ints = INT_MIN;
				}
				else
				{
					int v = (int)*doubles;
					int index = valueToIndex[v];
					*ints = index;
				}
			}

			_columnType = newColumnType;
		}
	}
	else if (newColumnType == Column::ColumnTypeScale)
	{
		if (_columnType == Column::ColumnTypeNominal || _columnType == ColumnTypeOrdinal)
		{
			Ints::iterator ints = this->AsInts.begin();
			Ints::iterator end = this->AsInts.end();
			Doubles::iterator doubles = this->AsDoubles.begin();

			for (; ints != end; ints++, doubles++)
			{
				int value = *ints;
				string display = stringFromRaw(value);
				double newValue;

				try
				{
					newValue = lexical_cast<double>(display);
				}
				catch (...)
				{
					newValue = NAN;
				}

				*doubles = newValue;
			}

			_columnType = newColumnType;
		}
		else if (_columnType == ColumnTypeNominalText)
		{
			Ints::iterator ints = AsInts.begin();
			Ints::iterator end = AsInts.end();
			Doubles::iterator doubles = AsDoubles.begin();

			for (; ints != end; ints++, doubles++)
			{
				try
				{
					*doubles = lexical_cast<double>(this->stringFromRaw(*ints));
				}
				catch (...)
				{
					*doubles = NAN;
				}
			}

			_columnType = newColumnType;
		}
	}

}

int Column::rowCount() const
{
	return _rowCount;
}

string Column::stringFromRaw(int value) const
{
	if (value == INT_MIN)
		return ".";

	if (_labels.size() > 0)
		return _labels.labelFor(value).text();

	stringstream ss;
	ss << value;

	return ss.str();
}

string Column::name() const
{
	return std::string(_name.begin(), _name.end());
}

void Column::setName(string name)
{
	_name = String(name.begin(), name.end(), _mem->get_segment_manager());
}

void Column::setValue(int rowIndex, string value)
{
	/*if (_dataType == Column::DataTypeDouble)
	{
		try
		{
			double d = boost::lexical_cast<double>(value);
			setValue(rowIndex, d);
		}
		catch (...)
		{
			//qDebug() << "could not assign: " << value.c_str();
		}
	}
	else
	{
		if (hasLabels() == false)
		{
			try
			{
				int v = boost::lexical_cast<int>(value);
				setValue(rowIndex, v);
			}
			catch (...)
			{
				//qDebug() << "could not assign: " << value.c_str();
			}
		}
	}*/
}

void Column::setValue(int rowIndex, int value)
{
	BlockMap::iterator itr = _blocks.upper_bound(rowIndex);

	if (itr == _blocks.end())
	{
		//qDebug() << "Column::setValue(), bad rowIndex";
		return;
	}

	int blockId = itr->first;
	DataBlock *block = itr->second.get();

	int blockIndex = rowIndex - blockId + DataBlock::capacity();
	block->Data[blockIndex].i = value;
}

void Column::setValue(int rowIndex, double value)
{
	BlockMap::iterator itr = _blocks.upper_bound(rowIndex);

	if (itr == _blocks.end())
	{
		//qDebug() << "Column::setValue(), bad rowIndex";
		return;
	}

	int blockId = itr->first;
	DataBlock *block = itr->second.get();

	int blockIndex = rowIndex - blockId + DataBlock::capacity();
	block->Data[blockIndex].d = value;
}

string Column::operator [](int index)
{
	if (_columnType == Column::ColumnTypeScale)
	{
		double v = AsDoubles[index];

		if (v > DBL_MAX)
		{
			char inf[] = { (char)0xE2, (char)0x88, (char)0x9E, 0 };
			return string(inf);
		}
		else if (v < -DBL_MAX)
		{
			char ninf[] = { (char)0x2D, (char)0xE2, (char)0x88, (char)0x9E, 0 };
			return string(ninf);
		}
		else if (std::isnan(v))
		{
			return ".";
		}
		else
		{
			stringstream s;
			s << v;
			return s.str();
		}
	}
	else
	{
		int value = AsInts[index];
		return stringFromRaw(value);
	}
}

void Column::append(int rows)
{
	if (rows == 0)
		return;

	BlockMap::reverse_iterator itr = _blocks.rbegin();

	if (itr == _blocks.rend()) // no blocks
	{
		ull firstId = DataBlock::capacity();
		DataBlock *firstBlock = _mem->construct<DataBlock>(anonymous_instance)();

		_blocks.insert(BlockEntry(firstId, firstBlock));
		itr = _blocks.rbegin();
	}

	BlockEntry entry = *itr;
	DataBlock *block = entry.second.get();
	ull id = entry.first;

	int room = DataBlock::capacity() - block->rowCount();
	int rowsLeft = rows - room;
	if (rowsLeft <= 0)
	{
		block->insert(block->rowCount(), rows);
		_rowCount += rows;
		return;
	}

	block->insert(block->rowCount(), room);
	_rowCount += room;

	int newBlocksRequired = rowsLeft / DataBlock::capacity();
	if (rowsLeft % DataBlock::capacity())
		newBlocksRequired++;

	for (int i = 0; i < newBlocksRequired; i++)
	{
		try {

		DataBlock *newBlock = _mem->construct<DataBlock>(anonymous_instance)();

		int toInsert = std::min(rowsLeft, DataBlock::capacity());
		newBlock->insert(0, toInsert);
		rowsLeft -= toInsert;

		id += DataBlock::capacity();
		_blocks.insert(BlockEntry(id, newBlock));

		_rowCount += toInsert;

		}
		catch (boost::interprocess::bad_alloc &e)
		{
			cout << e.what();
			cout << "setRowCount" << _rowCount << "\n";
			throw;
		}
	}
}

void Column::setColumnType(Column::ColumnType columnType)
{
	_columnType = columnType;
}

void Column::setRowCount(int rowCount)
{
	//std::cout << "setting row count, old " << _rowCount << " new " << rowCount << "\n";
	//std::cout.flush();

	if (rowCount == this->rowCount())
		return;

	if (rowCount > this->rowCount())
	{
		append(rowCount - this->rowCount());
		return;
	}

	_rowCount = rowCount;

	for (BlockMap::iterator itr = _blocks.upper_bound(rowCount); itr != _blocks.end(); itr++)
	{
		BlockEntry &entry = *itr;
		DataBlock *block = entry.second.get();
		_mem->destroy_ptr<DataBlock>(block);
		entry.second = NULL;
	}

	_blocks.erase(_blocks.upper_bound(rowCount), _blocks.end());

}

void Column::insert(int rowCount, int index)
{
	BlockMap::iterator itr = _blocks.lower_bound(index);

	// should check that itr != end()


}

Column::Ints::IntsStruct()
{
}

Column *Column::IntsStruct::getParent()
{
	Column *column = (Column*) NULL;
	char* intsAddress = (char*)&column->AsInts;
	char* baseAddress = (char*)column;
	char* thisAddress = (char*)this;

	return (Column*)(thisAddress - intsAddress + baseAddress);
}

int& Column::IntsStruct::operator [](int rowIndex)
{
	Column* parent = getParent();

	BlockMap::iterator itr = parent->_blocks.upper_bound(rowIndex);

	if (itr == parent->_blocks.end())
	{
		//qDebug() << "Column::Ints[], bad rowIndex";
	}

	int blockId = itr->first;
	DataBlock *block = itr->second.get();

	int pos = rowIndex - blockId + DataBlock::capacity();
	return block->Data[pos].i;
}

Column::Ints::iterator Column::Ints::begin()
{
	Column *parent = getParent();
	BlockMap::iterator itr = parent->_blocks.begin();
	return iterator(itr, 0);
}

Column::Ints::iterator Column::Ints::end()
{
	BlockMap::iterator itr = getParent()->_blocks.end();
	return iterator(itr, 0);
}

Column::Ints::iterator::iterator(BlockMap::iterator blockItr, int pos)
{
	_blockItr = blockItr;
	_currentPos = pos;
}

void Column::Ints::iterator::increment()
{
	_currentPos++;

	if (_currentPos >= _blockItr->second->rowCount())
	{
		_blockItr++;
		_currentPos = 0;
	}
}

bool Column::Ints::iterator::equal(iterator const& other) const
{
	return this->_currentPos == other._currentPos && this->_blockItr == other._blockItr;
}

int& Column::Ints::iterator::dereference() const
{
	return _blockItr->second->Data[_currentPos].i;
}

Column::Doubles::iterator Column::Doubles::begin()
{
	Column *parent = getParent();
	BlockMap::iterator itr = parent->_blocks.begin();
	return iterator(itr, 0);
}

Column::Doubles::iterator Column::Doubles::end()
{
	Column *parent = getParent();
	BlockMap::iterator itr = parent->_blocks.end();
	return iterator(itr, 0);
}

Column::Doubles::iterator::iterator(BlockMap::iterator blockItr, int pos)
{
	_blockItr = blockItr;
	_currentPos = pos;
}

Column::Doubles::DoublesStruct()
{
}

Column *Column::DoublesStruct::getParent()
{
	Column *column = (Column*) NULL;
	char* intsAddress = (char*)&column->AsDoubles;
	char* baseAddress = (char*)column;
	char* thisAddress = (char*)this;

	return (Column*)(thisAddress - intsAddress + baseAddress);
}

double& Column::DoublesStruct::operator [](int rowIndex)
{
	Column *parent = getParent();

	BlockMap::iterator itr = parent->_blocks.upper_bound(rowIndex);

	if (itr == parent->_blocks.end())
	{
		//qDebug() << "Column::Ints[], bad rowIndex";
	}

	int blockId = itr->first;
	DataBlock *block = itr->second.get();

	int pos = rowIndex - blockId + DataBlock::capacity();
	return block->Data[pos].d;
}

void Column::Doubles::iterator::increment()
{
	_currentPos++;

	if (_currentPos >= _blockItr->second->rowCount())
	{
		_blockItr++;
		_currentPos = 0;
	}
}

bool Column::Doubles::iterator::equal(iterator const& other) const
{
	return this->_currentPos == other._currentPos && this->_blockItr == other._blockItr;
}

double& Column::Doubles::iterator::dereference() const
{
	return _blockItr->second->Data[_currentPos].d;
}

