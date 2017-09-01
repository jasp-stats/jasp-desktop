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

#include "column.h"
#include "utils.h"

#include <boost/foreach.hpp>
#include <sstream>
#include <string>

#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <cmath>
#include <iostream>

using namespace boost::interprocess;
using namespace boost;
using namespace std;

int Column::count = 0;

bool Column::isEmptyValue(const string& val)
{
	if (val.empty()) return true;
	const vector<string>& emptyValues = Utils::getEmptyValues();
	return std::find(emptyValues.begin(), emptyValues.end(), val) != emptyValues.end();
}

bool Column::isEmptyValue(const double &val)
{
	if (isnan(val)) return true;
	const vector<double>& emptyValues = Utils::getDoubleEmptyValues();
	return std::find(emptyValues.begin(), emptyValues.end(), val) != emptyValues.end();
}

void Column::_convertVectorIntToDouble(vector<int> &intValues, vector<double> &doubleValues)
{
	doubleValues.clear();
	for (vector<int>::const_iterator it = intValues.begin(); it != intValues.end(); ++it)
	{
		const int &intValue = *it;
		double doubleValue = intValue;
		if (intValue == INT_MIN)
			doubleValue == NAN;
		doubleValues.push_back(doubleValue);
	}

}

bool Column::_resetEmptyValuesForNominal(std::map<int, string> &emptyValuesMap)
{
	bool hasChanged = false;
	std::map<int, string> emptyValuesMapOrg = emptyValuesMap;
	int row = 0;
	bool hasEmptyValues = !emptyValuesMap.empty();
	bool changeToNominalText = false;
	Ints::iterator ints = AsInts.begin();
	Ints::iterator end = AsInts.end();

	set<int> uniqueValues = _labels.getIntValues();

	for (; ints != end; ints++)
	{
		int intValue = *ints;
		if (intValue == INT_MIN && hasEmptyValues)
		{
			auto search = emptyValuesMap.find(row);
			if (search != emptyValuesMap.end())
			{
				string orgValue = search->second;
				if (!isEmptyValue(orgValue))
				{
					// This value is not empty anymore
					try
					{
						intValue = boost::lexical_cast<int>(orgValue);
						*ints = intValue;
						uniqueValues.insert(intValue);
						hasChanged = true;
						emptyValuesMap.erase(search);
					}
					catch (...)
					{
						// The original value is not an integer, this column cannot be nominal anymore
						// Let's make it a nominal text.
						changeToNominalText = true;
						break;
					}
				}
			}
		}
		else if (intValue != INT_MIN && isEmptyValue(intValue))
		{
			// This value is now considered as empty
			*ints = INT_MIN;
			uniqueValues.erase(intValue);
			hasChanged = true;
			std::ostringstream strs;
			strs << intValue;
			emptyValuesMap.insert(make_pair(row, strs.str()));
		}
		row++;
	}

	if (changeToNominalText)
	{
		setColumnType(Column::ColumnTypeNominalText);
		emptyValuesMap.clear();
		emptyValuesMap.insert(emptyValuesMapOrg.begin(), emptyValuesMapOrg.end());
		hasChanged = _resetEmptyValuesForNominalText(emptyValuesMap, false);
	}
	else
		_labels.syncInts(uniqueValues);

	return hasChanged;
}

bool Column::_resetEmptyValuesForScale(std::map<int, string> &emptyValuesMap)
{
	bool hasChanged = false;
	int row = 0;
	bool hasEmptyValues = !emptyValuesMap.empty();
	bool changeToNominalText = false;
	Doubles::iterator doubles = AsDoubles.begin();
	Doubles::iterator end = AsDoubles.end();

	for (; doubles != end; doubles++)
	{
		double doubleValue = *doubles;
		if (doubleValue == NAN && hasEmptyValues)
		{
			auto search = emptyValuesMap.find(row);
			if (search != emptyValuesMap.end())
			{
				string orgValue = search->second;
				if (!isEmptyValue(orgValue))
				{
					// This value is not empty anymore
					try
					{
						doubleValue = boost::lexical_cast<double>(orgValue);
						*doubles = doubleValue;
						hasChanged = true;
					}
					catch (...)
					{
						changeToNominalText = true;
						break;
					}
				}
			}
		}
		else if (doubleValue != NAN && isEmptyValue(doubleValue))
		{
			// This value is now considered as empty
			*doubles = NAN;
			hasChanged = true;
			std::ostringstream strs;
			strs << doubleValue;
			emptyValuesMap.insert(make_pair(row, strs.str()));
		}
		row++;
	}

	if (changeToNominalText)
	{
		// Cannot use _resetEmptyValuesForNominalText since the AsInts are not set.
		vector<string> values;
		row = 0;
		for (doubles = AsDoubles.begin(); doubles != end; doubles++)
		{
			double doubleValue = *doubles;
			if (doubleValue == NAN)
			{
				auto search = emptyValuesMap.find(row);
				if (search != emptyValuesMap.end())
				{
					string orgValue = search->second;
					values.push_back(orgValue);
				}
				else
				{
					values.push_back(Utils::emptyValue);
				}
			}
			else
			{
				std::ostringstream strValue;
				strValue << doubleValue;
				values.push_back(strValue.str());
			}
			row++;
		}
		map<int, string> newEmptyValues = setColumnAsNominalString(values);
		emptyValuesMap.clear();
		emptyValuesMap.insert(newEmptyValues.begin(), newEmptyValues.end());
		hasChanged = true;
	}

	return hasChanged;
}

bool Column::_resetEmptyValuesForNominalText(std::map<int, string> &emptyValuesMap, bool tryToConvert)
{
	bool hasChanged = false;
	int row = 0;
	bool hasEmptyValues = !emptyValuesMap.empty();
	Ints::iterator ints = AsInts.begin();
	Ints::iterator end = AsInts.end();
	vector<string> values;
	vector<int> intValues;
	vector<double> doubleValues;
	set<int> uniqueIntValues;
	bool canBeConvertedToIntegers = tryToConvert, canBeConvertedToDoubles = tryToConvert;

	for (; ints != end; ints++)
	{
		int index = *ints;
		if (index == INT_MIN && hasEmptyValues)
		{
			auto search = emptyValuesMap.find(row);
			if (search != emptyValuesMap.end())
			{
				string orgValue = search->second;
				values.push_back(orgValue);
				if (!isEmptyValue(orgValue))
					hasChanged = true;
				if (canBeConvertedToIntegers || canBeConvertedToDoubles)
				{
					if (isEmptyValue(orgValue))
					{
						if (canBeConvertedToIntegers)
							intValues.push_back(INT_MIN);
						else
							doubleValues.push_back(NAN);
					}
					else
					{
						bool convertToDouble = false;
						if (canBeConvertedToIntegers)
						{
							try
							{
								int intValue = boost::lexical_cast<int>(orgValue);
								intValues.push_back(intValue);
								uniqueIntValues.insert(intValue);
								emptyValuesMap.erase(search);
							}
							catch (...)
							{
								canBeConvertedToIntegers = false;
								_convertVectorIntToDouble(intValues, doubleValues);
								convertToDouble = true;
							}
						}
						else
						{
							convertToDouble = true;
						}

						if (convertToDouble)
						{
							try
							{
								double doubleValue = boost::lexical_cast<double>(orgValue);
								doubleValues.push_back(doubleValue);
								emptyValuesMap.erase(search);
							}
							catch (...)
							{
								canBeConvertedToDoubles = false;
							}
						}
					}
				}
			}
			else
			{
				values.push_back(Utils::emptyValue);
				if (canBeConvertedToIntegers)
					intValues.push_back(INT_MIN);
				else if (canBeConvertedToDoubles)
					doubleValues.push_back(NAN);
			}
		}
		else if (index == INT_MIN)
		{
			values.push_back(Utils::emptyValue);
			if (canBeConvertedToIntegers)
				intValues.push_back(INT_MIN);
			else if (canBeConvertedToDoubles)
				doubleValues.push_back(NAN);
		}
		else
		{
			string orgValue = _labels.getValueFromIndex(index);
			values.push_back(orgValue);
			if (isEmptyValue(orgValue))
			{
				hasChanged = true;
				if (canBeConvertedToIntegers)
				{
					intValues.push_back(INT_MIN);
					emptyValuesMap.insert(make_pair(row, orgValue));
				}
				else if (canBeConvertedToDoubles)
				{
					doubleValues.push_back(NAN);
					emptyValuesMap.insert(make_pair(row, orgValue));
				}
			}
			else
			{
				bool convertToDouble = false;
				if (canBeConvertedToIntegers)
				{
					try
					{
						int intValue = boost::lexical_cast<int>(orgValue);
						intValues.push_back(intValue);
						uniqueIntValues.insert(intValue);
					}
					catch (...)
					{
						canBeConvertedToIntegers = false;
						_convertVectorIntToDouble(intValues, doubleValues);
						convertToDouble = true;
					}
				}
				else if (canBeConvertedToDoubles)
				{
					convertToDouble = true;
				}

				if (convertToDouble)
				{
					try
					{
						double doubleValue = boost::lexical_cast<double>(orgValue);
						doubleValues.push_back(doubleValue);
					}
					catch (...)
					{
						canBeConvertedToDoubles = false;
					}
				}
			}
		}
		row++;
	}

	if (canBeConvertedToIntegers)
	{
		setColumnAsNominalOrOrdinal(intValues, uniqueIntValues);
		hasChanged = true;
	}
	else if (canBeConvertedToDoubles)
	{
		setColumnAsScale(doubleValues);
		hasChanged = true;
	}
	else
	{
		map<int, string> newEmptyValues = setColumnAsNominalString(values);
		emptyValuesMap.clear();
		emptyValuesMap.insert(newEmptyValues.begin(), newEmptyValues.end());
	}

	return hasChanged;

}

bool Column::resetEmptyValues(std::map<int, string> &emptyValuesMap)
{
	if (_columnType == Column::ColumnTypeOrdinal || _columnType == Column::ColumnTypeNominal)
		return _resetEmptyValuesForNominal(emptyValuesMap);
	else if (_columnType == Column::ColumnTypeScale)
		return _resetEmptyValuesForScale(emptyValuesMap);
	else
		return _resetEmptyValuesForNominalText(emptyValuesMap);
}

Column::Column(managed_shared_memory *mem) :
	_name(mem->get_segment_manager()),
	_blocks(std::less<ull>(), mem->get_segment_manager()),
	_labels(mem)
{
	id = ++count;
	_mem = mem;
	_rowCount = 0;
	_columnType = Column::ColumnTypeNominal;
}

Column::Column(const Column &col) :
	_name(col._name),
	_blocks(col._blocks),
	_labels(col._labels)
{
	id = ++count;
	_mem = col._mem;
	_rowCount = col._rowCount;
	_columnType = col._columnType;
}

Column::~Column()
{
	//BOOST_FOREACH(BlockEntry &entry, _blocks)
		//_mem->destroy_ptr(&*entry.second);
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

	bool success = true;
	if (newColumnType == ColumnTypeOrdinal || newColumnType == ColumnTypeNominal)
	{
		if (_columnType == ColumnTypeNominal || _columnType == ColumnTypeOrdinal)
		{
			_columnType = newColumnType;
		}
		else
		{
			vector<int> values;
			set<int> uniqueValues;
			if (_columnType == ColumnTypeNominalText)
			{
				Ints::iterator intIterator = AsInts.begin();
				for (; intIterator != AsInts.end(); intIterator++)
				{
					int value = *intIterator;
					string display = _labelFromIndex(value);
					try
					{
						if (isEmptyValue(display))
						{
							values.push_back(INT_MIN);
						}
						else
						{
							int value = lexical_cast<int>(display);
							values.push_back(value);
							uniqueValues.insert(value);
						}
					}
					catch (...)
					{
						success = false;
						break;
					}
				}
			}
			else if (_columnType == ColumnTypeScale)
			{
				Doubles::iterator doubles = this->AsDoubles.begin();
				for (; doubles != this->AsDoubles.end(); doubles++)
				{
					try
					{
						if (*doubles != NAN)
						{
							int v = lexical_cast<int>(*doubles);
							uniqueValues.insert(v);
							values.push_back(v);
						}
						else
						{
							values.push_back(INT_MIN);
						}
					}
					catch (...)
					{
						success = false;
						break;
					}
				}
			}

			if (success)
			{
				setColumnAsNominalOrOrdinal(values, uniqueValues, newColumnType == ColumnTypeOrdinal);
			}
			else if (_columnType == ColumnTypeScale && newColumnType == ColumnTypeNominal) // set the column as ColumnTypeNominalText
			{
				vector<string> values;
				Doubles::iterator doubles = this->AsDoubles.begin();
				for (; doubles != this->AsDoubles.end(); doubles++)
				{
					std::ostringstream strs;
					strs << *doubles;
					values.push_back(strs.str());
				}
				setColumnAsNominalString(values);
			}
		}
	}
	else if (newColumnType == Column::ColumnTypeScale)
	{
		vector<double> values;
		Ints::iterator ints = AsInts.begin();
		Ints::iterator end = AsInts.end();

		if (_columnType == Column::ColumnTypeNominal || _columnType == Column::ColumnTypeOrdinal)
		{
			for (; ints != end; ints++)
			{
				int intValue = *ints;
				double doubleValue;

				if (intValue == INT_MIN)
					doubleValue = NAN;
				else
					doubleValue = (double)intValue;

				values.push_back(doubleValue);
			}
		}
		else if (_columnType == ColumnTypeNominalText)
		{
			for (; ints != end; ints++)
			{
				try
				{
					string value = this->_labelFromIndex(*ints);
					double doubleValue;

					if (isEmptyValue(value))
						doubleValue = NAN;
					else
						doubleValue = lexical_cast<double>(value);

					values.push_back(doubleValue);
				}
				catch (...)
				{
					success = false;
					break;
				}
			}
		}

		if (success)
			setColumnAsScale(values);
	}
}

void Column::setColumnAsNominalOrOrdinal(const vector<int> &values, map<int, string> &uniqueValues, bool is_ordinal)
{
	_labels.syncInts(uniqueValues);
	_setColumnAsNominalOrOrdinal(values, is_ordinal);
}

void Column::setColumnAsNominalOrOrdinal(const vector<int> &values, const set<int> &uniqueValues, bool is_ordinal)
{
	_labels.syncInts(uniqueValues);
	_setColumnAsNominalOrOrdinal(values, is_ordinal);
}

void Column::_setColumnAsNominalOrOrdinal(const vector<int> &values, bool is_ordinal)
{
	Ints::iterator intInputItr = AsInts.begin();
	int nb_values = 0;

	BOOST_FOREACH(int value, values)
	{
		*intInputItr = value;
		intInputItr++;
		nb_values++;
	}

	while (nb_values < _rowCount)
	{
		*intInputItr = INT_MIN;
		intInputItr++;
		nb_values++;
	}

	setColumnType(is_ordinal ? Column::ColumnTypeOrdinal : Column::ColumnTypeNominal);

}

void Column::setColumnAsScale(const std::vector<double> &values)
{
	_labels.clear();
	Doubles::iterator doubleInputItr = AsDoubles.begin();

	BOOST_FOREACH(double value, values)
	{
		*doubleInputItr = value;
		doubleInputItr++;
	}

	setColumnType(Column::ColumnTypeScale);

}

map<int, string> Column::setColumnAsNominalString(const vector<string> &values)
{
	return setColumnAsNominalString(values, map<string, string>());
}

map<int, string> Column::setColumnAsNominalString(const vector<string> &values, const map<string, string>&labels)
{
	map<int, string> emptyValuesMap;

	vector<string> sorted = values;
	sort(sorted.begin(), sorted.end());
	vector<string> cases;
	unique_copy(sorted.begin(), sorted.end(), back_inserter(cases));
	sort(cases.begin(), cases.end());

	cases.erase(std::remove_if(
					cases.begin(),
					cases.end(),
					[](string x){
						return isEmptyValue(x);
					}), cases.end());

	std::map<string, int> map = _labels.syncStrings(cases, labels);

	Column::Ints::iterator intInputItr = AsInts.begin();
	int nb_values = 0;

	BOOST_FOREACH (const string &value, values)
	{
		if (isEmptyValue(value))
		{
			*intInputItr = INT_MIN;
			if (!value.empty())
				emptyValuesMap.insert(make_pair(nb_values, value));
		}
		else
		{
			*intInputItr = map[value];
		}
		intInputItr++;
		nb_values++;
	}

	while (nb_values < _rowCount)
	{
		*intInputItr = INT_MIN;
		intInputItr++;
		nb_values++;
	}

	setColumnType(Column::ColumnTypeNominalText);

	return emptyValuesMap;
}

int Column::rowCount() const
{
	return _rowCount;
}

string Column::_labelFromIndex(int value) const
{
	if (value == INT_MIN)
		return Utils::emptyValue;

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

bool Column::isValueEqual(int rowIndex, double value)
{
	if (rowIndex >= _rowCount)
		return false;

	if (_columnType == Column::ColumnTypeScale)
	{
		double d = AsDoubles[rowIndex];
		if (isEmptyValue(value))
			return isEmptyValue(d);
		else
			return d == value;
	}

	return false;
}

bool Column::isValueEqual(int rowIndex, int value)
{
	if (rowIndex >= _rowCount)
		return false;

	if (_columnType == Column::ColumnTypeScale)
		return AsDoubles[rowIndex] == value;

	int intValue = AsInts[rowIndex];
	if (_columnType == Column::ColumnTypeNominal || _columnType == Column::ColumnTypeOrdinal)
	{
		bool result = (intValue == value);
		if (!result)
		{
			std::cout << "Value not equal: " << intValue << " " << value << std::endl;
			std::cout.flush();
		}
		return result;
	}

	if (intValue == INT_MIN)
		return value == INT_MIN;

	if (_labels.size() > 0)
	{
		Label label = _labels.labelFor(intValue);
		if (label.hasIntValue())
			return label.value() == value;
	}
	return false;
}

bool Column::isValueEqual(int rowIndex, const string &value)
{
	if (rowIndex >= _rowCount)
		return false;

	bool result = false;
	switch (_columnType)
	{
		case Column::ColumnTypeScale:
		{
			double v = AsDoubles[rowIndex];
			stringstream s;
			s << v;
			string str = s.str();
			result = (str == value);
			break;
		}

		case Column::ColumnTypeNominal:
		case Column::ColumnTypeOrdinal:
		{
			int v = AsInts[rowIndex];
			stringstream s;
			s << v;
			string str = s.str();
			result = (str == value);
			break;
		}
		default:
		{
			int index = AsInts[rowIndex];
			if (index == INT_MIN)
			{
				result = isEmptyValue(value);
			}
			else
			{
				result = (value.substr(0, 128) == _labels.getValueFromIndex(index));
			}
		}

	}

	return result;
}

string Column::_getScaleValue(int row)
{
	double v = AsDoubles[row];

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
	else if (isEmptyValue(v))
	{
		return Utils::emptyValue;
	}
	else
	{
		stringstream s;
		s << v;
		return s.str();
	}
}

string Column::getOriginalValue(int row)
{
	string result = Utils::emptyValue;

	if (row < _rowCount)
	{
		if (_columnType == Column::ColumnTypeScale)
		{
			result = _getScaleValue(row);
		}
		else
		{
			int index = AsInts[row];
			if (index == INT_MIN)
				result = Utils::emptyValue;
			else
				result = _labels.getValueFromIndex(index);
		}
	}

	return result;
}


string Column::operator [](int row)
{
	string result = Utils::emptyValue;

	if (row < _rowCount)
	{
		if (_columnType == Column::ColumnTypeScale)
		{
			result = _getScaleValue(row);
		}
		else
		{
			int value = AsInts[row];
			result = _labelFromIndex(value);
		}
	}

	return result;
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
		block->insert(rows);
		_rowCount += rows;
		return;
	}

	block->insert(room);
	_rowCount += room;

	int newBlocksRequired = rowsLeft / DataBlock::capacity();
	if (rowsLeft % DataBlock::capacity())
		newBlocksRequired++;

	for (int i = 0; i < newBlocksRequired; i++)
	{
		try {

		DataBlock *newBlock = _mem->construct<DataBlock>(anonymous_instance)();

		int toInsert = std::min(rowsLeft, DataBlock::capacity());
		newBlock->insert(toInsert);
		rowsLeft -= toInsert;

		id += DataBlock::capacity();
		_blocks.insert(BlockEntry(id, newBlock));

		_rowCount += toInsert;

		}
		catch (boost::interprocess::bad_alloc &e)
		{
			cout << e.what() << " ";
			cout << "append column " << name() << ", append: " << rows << ", rowCount: " << _rowCount << std::endl;
			throw e;
		}
	}
}

void Column::truncate(int rows)
{
	if (rows <= 0) return;

	BlockMap::reverse_iterator itr = _blocks.rbegin();
	DataBlock *block = itr->second.get();

	int rowsToDelete = rows;

	if (rowsToDelete > _rowCount)
		rowsToDelete = _rowCount;

	while (rowsToDelete > 0)
	{
		if (block->rowCount() >= rowsToDelete)
		{
			block->erase(rowsToDelete);
			_rowCount -= rowsToDelete;
			rowsToDelete = 0;
		}
		else
		{
			rowsToDelete -= block->rowCount();
			_rowCount -= block->rowCount();
			block->erase(block->rowCount());
			//_mem->destroy_ptr<DataBlock>(block);
			itr++;
			if (itr == _blocks.rend())
			{
				std::cout << "Try to erase more blocks than existing!!" << std::endl;
				std::cout.flush();
				rowsToDelete = 0;
				_rowCount = 0;
			}
			else
			{
				block = itr->second.get();
			}
		}
	}
}

void Column::setColumnType(Column::ColumnType columnType)
{
	_columnType = columnType;
}

void Column::_setRowCount(int rowCount)
{
	if (rowCount > this->rowCount())
		append(rowCount - this->rowCount());
	else if (rowCount < this->rowCount())
		truncate(this->rowCount() - rowCount);
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
		std::cout << "Column::Ints[], bad rowIndex: " << rowIndex << std::endl;
		std::cout << "Nb of blocks: " << parent->_blocks.size() << std::endl;
		std::cout.flush();
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
