
#include "column.h"

#include <boost/foreach.hpp>
#include <sstream>
#include <string>

#include <boost/lexical_cast.hpp>
#include <cmath>

using namespace boost::interprocess;
using namespace boost;
using namespace std;

Column::Column() :
	_name(SharedMemory::get()->get_segment_manager()),
	_blocks(std::less<ull>(), SharedMemory::get()->get_segment_manager())
{
	_labels = NULL;
	_rowCount = 0;
	_columnType = Column::ColumnTypeNominal;

	ull firstId = DataBlock::capacity();
	DataBlock *firstBlock = SharedMemory::get()->construct<DataBlock>(anonymous_instance)();

	_blocks.insert(BlockEntry(firstId, firstBlock));
}

std::map<int, std::string> Column::labels() const
{
	typedef std::pair<const int, std::string> label;

	std::map<int, std::string> ls;

	Labels *source = _labels.get();

	if (source != NULL)
	{
		BOOST_FOREACH(LabelEntry &entry, *source)
		{
			std::string value(entry.second.begin(), entry.second.end());
			ls.insert(label(entry.first, value));
		}
	}

	return ls;
}

void Column::setLabels(std::map<int, std::string> labels)
{
	managed_shared_memory *mem = SharedMemory::get();

	if (_labels == NULL)
		_labels = mem->construct<Labels>(anonymous_instance)(std::less<int>(), mem->get_segment_manager());
	else
		_labels->clear();

	typedef std::pair<const int, std::string> label;

	BOOST_FOREACH(label &p, labels)
	{
		int rawValue = p.first;
		String displayValue(p.second.begin(), p.second.end(), mem->get_segment_manager());
		_labels->insert(LabelEntry(rawValue, displayValue));
	}

	if (_numericLabels != NULL)
	{
		mem->destroy_ptr(_numericLabels.get());
		_numericLabels = NULL;
	}
}

bool Column::hasNumericLabels() const
{
	return _numericLabels.get() != NULL;
}

void Column::setLabels(std::map<int, int> labels)
{
	managed_shared_memory *mem = SharedMemory::get();

	if (_numericLabels == NULL)
		_numericLabels = mem->construct<NumericLabels>(anonymous_instance)(std::less<int>(), mem->get_segment_manager());
	else
		_numericLabels->clear();

	typedef std::pair<const int, int> label;

	BOOST_FOREACH(label &p, labels)
	{
		int rawValue = p.first;
		int actualValue = p.second;
		_numericLabels->insert(NumericEntry(rawValue, actualValue));
	}

	if (_labels != NULL)
	{
		mem->destroy_ptr(_labels.get());
		_labels = NULL;
	}
}

std::map<int, int> Column::numericLabels() const
{
	typedef std::pair<const int, int> label;

	std::map<int, int> ls;

	NumericLabels *source = _numericLabels.get();

	if (source != NULL)
	{
		BOOST_FOREACH(NumericEntry &entry, *source)
			ls.insert(label(entry.first, entry.second));
	}

	return ls;
}

int Column::actualFromRaw(int raw) const
{
	if (raw != INT_MIN && hasNumericLabels())
		return _numericLabels->at(raw);
	else
		return raw;
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
			map<int, string> labels = this->labels();
			map<int, int> oldIndexToNewIndex;
			map<int, int> numericLabels;

			typedef pair<const int, string> label;

			int index = 0;

			BOOST_FOREACH (label l, labels)
			{
				try
				{
					int value = lexical_cast<int>(l.second);
					numericLabels[index] = value;
					oldIndexToNewIndex[l.first] = index;
					index++;
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

			setLabels(numericLabels);

			_columnType = newColumnType;
		}
		else if (_columnType == ColumnTypeScale)
		{
			std::set<int> uniqueValues;

			Doubles::iterator doubles = this->AsDoubles.begin();
			for (; doubles != this->AsDoubles.end(); doubles++)
			{
				int v = (int)*doubles;
				if ( ! isnan(v))
					uniqueValues.insert(v);
			}

			map<int, int> labels;
			map<int, int> valueToIndex;

			int index = 0;
			set<int>::iterator itr = uniqueValues.begin();
			for (; itr != uniqueValues.end(); itr++)
			{
				int v = *itr;

				labels[index] = v;
				valueToIndex[v] = index;

				index++;
			}

			Ints::iterator ints = this->AsInts.begin();
			Ints::iterator end = this->AsInts.end();
			doubles = this->AsDoubles.begin();

			for (; ints != end; ints++, doubles++)
			{
				double value = *doubles;
				if (isnan(value))
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

			setLabels(labels);

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

			SharedMemory::get()->destroy_ptr(_numericLabels.get());
			_numericLabels = NULL;

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
					*doubles = lexical_cast<int>(this->stringFromRaw(*ints));
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

bool Column::hasLabels() const
{
	return _labels.get() != NULL;
}

string Column::stringFromRaw(int value) const
{
	if (value == INT_MIN)
		return ".";

	if (hasLabels())
	{
		String s = _labels->at(value);
		return string(s.begin(), s.end());
	}

	if (hasNumericLabels())
	{
		value = _numericLabels->at(value);
	}

	stringstream s;
	s << value;

	return s.str();
}

string Column::name() const
{
	return std::string(_name.begin(), _name.end());
}

void Column::setName(string name)
{
	_name = String(name.begin(), name.end(), SharedMemory::get()->get_segment_manager());
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
		else if (isnan(v))
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
	BlockMap::reverse_iterator itr = _blocks.rbegin();
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

		DataBlock *newBlock = SharedMemory::get()->construct<DataBlock>(anonymous_instance)();

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
		SharedMemory::get()->destroy_ptr<DataBlock>(block);
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

