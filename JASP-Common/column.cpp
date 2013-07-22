
#include "column.h"

#include <boost/foreach.hpp>
#include <sstream>

#include <boost/lexical_cast.hpp>

using namespace boost::interprocess;

Column::Column(managed_shared_memory *mem) :
	//AsInts(this),
	//AsDoubles(this),
	_name(mem->get_segment_manager()),
	_blocks(std::less<ull>(), mem->get_segment_manager())
{
	_mem = mem;
	_labels = NULL;
	_columnType = Column::IntColumnType;
	_rowCount = 0;

	ull firstId = DataBlock::capacity();
	DataBlock *firstBlock = mem->construct<DataBlock>(anonymous_instance)();

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
}

Column::ColumnType Column::columnType() const
{
	return _columnType;
}

int Column::rowCount() const
{
	return _rowCount;
}

bool Column::hasLabels()
{
	return _labels.get() != NULL;
}

std::string Column::displayFromValue(int value)
{
	if (hasLabels())
	{
		String s = _labels->at(value);
		return string(s.begin(), s.end());
	}

	if (value == INT_MIN)
		return "";

	stringstream s;
	s << value;

	return s.str();
}

string Column::name()
{
	return std::string(_name.begin(), _name.end());
}

void Column::setName(string name)
{
	_name = String(name.begin(), name.end(), _mem->get_segment_manager());
}

void Column::setValue(int rowIndex, string value)
{
	if (_columnType == Column::DoubleColumnType)
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
	}
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
	if (_columnType == Column::DoubleColumnType)
	{
		stringstream s;
		s << AsDoubles[index];
		return s.str();
	}
	else
	{
		int value = AsInts[index];
		return displayFromValue(value);
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

	int newBlocksRequired = rowsLeft / DataBlock::capacity();
	if (rowsLeft % DataBlock::capacity())
		newBlocksRequired++;

	for (int i = 0; i < newBlocksRequired; i++)
	{
		DataBlock *newBlock = _mem->construct<DataBlock>(anonymous_instance)();
		int toInsert = std::min(rowsLeft, DataBlock::capacity());
		newBlock->insert(0, toInsert);
		rowsLeft -= toInsert;

		id += DataBlock::capacity();
		_blocks.insert(BlockEntry(id, newBlock));
	}

	_rowCount += rows;
}

void Column::setRowCount(int rowCount)
{
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


/*Column::ColumnLabels::ColumnLabelsStruct(Column *parent)
{
	_parent = parent;
}

Column::ColumnLabels::iterator Column::ColumnLabels::begin()
{
	string* start = (string*) _parent->data();

	iterator itr(start);

	return itr;
}

Column::ColumnLabels::iterator Column::ColumnLabels::end()
{
	string *end = (string*) _parent->data();
	end += _parent->rowCount();

	iterator itr(end);

	return itr;
}

Column::ColumnLabels::iterator::iterator()
{
	_current = 0;
}

Column::ColumnLabels::iterator::iterator(int index)
{
	_current = index;
}

void Column::ColumnLabels::iterator::increment()
{
	_current++;
}

bool Column::ColumnLabels::iterator::equal(iterator const& other) const
{
	return this->_current == other._current;
}

string& Column::ColumnLabels::iterator::dereference() const
{
	return _parent.;
}*/
