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

Labels &Column::labels()
{
	return _labels;
}

bool Column::isEmptyValue(const string& val)
{
	if (val.empty()) return true;
	const vector<string>& emptyValues = Utils::getEmptyValues();
	return std::find(emptyValues.begin(), emptyValues.end(), val) != emptyValues.end();
}

bool Column::isEmptyValue(const double &val)
{
	if (std::isnan(val)) return true;
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
			doubleValue = NAN;
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
					if (Utils::getIntValue(orgValue, intValue))
					{
						*ints = intValue;
						uniqueValues.insert(intValue);
						hasChanged = true;
						emptyValuesMap.erase(search);
					}
					else
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
	else if (hasChanged)
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
		if (std::isnan(doubleValue) && hasEmptyValues)
		{
			auto search = emptyValuesMap.find(row);
			if (search != emptyValuesMap.end())
			{
				string orgValue = search->second;
				if (!isEmptyValue(orgValue))
				{
					// This value is not empty anymore
					if (Utils::getDoubleValue(orgValue, doubleValue))
					{
						*doubles = doubleValue;
						hasChanged = true;
					}
					else
					{
						changeToNominalText = true;
						break;
					}
				}
			}
		}
		else if (!std::isnan(doubleValue) && isEmptyValue(doubleValue))
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
		// So use setColumnAsNominalText
		vector<string> values;
		row = 0;
		for (doubles = AsDoubles.begin(); doubles != end; doubles++)
		{
			double doubleValue = *doubles;
			if (std::isnan(doubleValue))
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
		map<int, string> newEmptyValues = setColumnAsNominalText(values);
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
	map<int, string> intLabels;
	bool canBeConvertedToIntegers = tryToConvert, canBeConvertedToDoubles = tryToConvert;

	for (; ints != end; ints++)
	{
		int key = *ints;
		if (key == INT_MIN && hasEmptyValues)
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
							int intValue;
							if (Utils::getIntValue(orgValue, intValue))
							{
								intValues.push_back(intValue);
								if (uniqueIntValues.find(intValue) == uniqueIntValues.end())
								{
									uniqueIntValues.insert(intValue);
									intLabels.insert(make_pair(intValue, orgValue));
								}
								emptyValuesMap.erase(search);
							}
							else
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
							double doubleValue;
							if (Utils::getDoubleValue(orgValue, doubleValue))
							{
								doubleValues.push_back(doubleValue);
								emptyValuesMap.erase(search);
							}
							else
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
		else if (key == INT_MIN)
		{
			values.push_back(Utils::emptyValue);
			if (canBeConvertedToIntegers)
				intValues.push_back(INT_MIN);
			else if (canBeConvertedToDoubles)
				doubleValues.push_back(NAN);
		}
		else
		{
			string orgValue = _labels.getValueFromKey(key);
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
					int intValue;
					if (Utils::getIntValue(orgValue, intValue))
					{
						intValues.push_back(intValue);
						if (uniqueIntValues.find(intValue) == uniqueIntValues.end())
						{
							uniqueIntValues.insert(intValue);
							intLabels.insert(make_pair(intValue, _getLabelFromKey(key)));
						}
					}
					else
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
					double doubleValue;
					if (Utils::getDoubleValue(orgValue, doubleValue))
						doubleValues.push_back(doubleValue);
					else
						canBeConvertedToDoubles = false;
				}
			}
		}
		row++;
	}

	if (canBeConvertedToIntegers)
	{
		_labels.clear();
		setColumnAsNominalOrOrdinal(intValues, intLabels);
		hasChanged = true;
	}
	else if (canBeConvertedToDoubles)
	{
		setColumnAsScale(doubleValues);
		hasChanged = true;
	}
	else if (hasChanged)
	{
		map<int, string> newEmptyValues = setColumnAsNominalText(values);
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

void Column::setSharedMemory(managed_shared_memory *mem)
{
	_mem = mem;
	_labels.setSharedMemory(mem);
}

Column::ColumnType Column::columnType() const
{
	return _columnType;
}

bool Column::_changeColumnToNominalOrOrdinal(ColumnType newColumnType)
{
	bool success = true;
	if (_columnType == ColumnTypeNominal || _columnType == ColumnTypeOrdinal)
	{
		_columnType = newColumnType;
	}
	else if (_columnType == ColumnTypeNominalText)
	{
		vector<int> values;
		set<int> uniqueIntValues;
		map<int, string> intLabels;
		Ints::iterator intIterator = AsInts.begin();
		for (; intIterator != AsInts.end(); intIterator++)
		{
			int intValue = INT_MIN;
			int key = *intIterator;
			if (key != INT_MIN)
			{
				string value = _labels.getValueFromKey(key);
				if (!isEmptyValue(value))
					success = Utils::getIntValue(value, intValue);
			}

			if (!success)
				break;

			values.push_back(intValue);
			if (intValue != INT_MIN)
			{
				if (uniqueIntValues.find(intValue) != uniqueIntValues.end())
				{
					string label = _getLabelFromKey(key);
					uniqueIntValues.insert(intValue);
					intLabels.insert(make_pair(intValue, label));
				}
			}
		}

		if (success)
		{
			_labels.clear();
			setColumnAsNominalOrOrdinal(values, intLabels, newColumnType == ColumnType::ColumnTypeOrdinal);
		}
		else if (newColumnType == ColumnType::ColumnTypeNominal)
			// ColumnTypeNominalText to ColumnTypeNominal: we could not make the values as integers, but
			// the column can still stay a NominalText, so it is not a failure.
			success = true;

	}
	else if (_columnType == ColumnTypeScale)
	{
		vector<int> values;
		set<int> uniqueIntValues;
		Doubles::iterator doubles = this->AsDoubles.begin();
		for (; doubles != this->AsDoubles.end(); doubles++)
		{
			int intValue = INT_MIN;
			double doubleValue = *doubles;
			if (!isEmptyValue(doubleValue))
				success = Utils::getIntValue(doubleValue, intValue);

			if (!success)
				break;

			values.push_back(intValue);
			if (intValue != INT_MIN)
				uniqueIntValues.insert(intValue);
		}

		if (success)
			setColumnAsNominalOrOrdinal(values, uniqueIntValues, newColumnType == ColumnTypeOrdinal);
		else
		{
			vector<string> values;
			Doubles::iterator doubles = AsDoubles.begin();
			for (; doubles != AsDoubles.end(); doubles++)
			{
				double doubleValue = *doubles;
				if (std::isnan(doubleValue))
				{
					values.push_back(Utils::emptyValue);
				}
				else
				{
					std::ostringstream strs;
					strs << doubleValue;
					values.push_back(strs.str());
				}
			}
			setColumnAsNominalText(values);
			success = true;
		}
	}

	return success;
}

bool Column::_changeColumnToScale()
{
	bool success = true;
	vector<double> values;
	Ints::iterator ints = AsInts.begin();
	Ints::iterator end = AsInts.end();

	if (_columnType == Column::ColumnTypeNominal || _columnType == Column::ColumnTypeOrdinal)
	{
		for (; ints != end; ints++)
		{
			int intValue = *ints;
			double doubleValue = NAN;

			if (intValue != INT_MIN && !isEmptyValue(intValue))
				doubleValue = (double)intValue;

			values.push_back(doubleValue);
		}
	}
	else if (_columnType == ColumnTypeNominalText)
	{
		for (; ints != end; ints++)
		{
			double doubleValue = NAN;
			int key = *ints;
			if (key != INT_MIN)
			{
				string value = _labels.getValueFromKey(key);
				if (!isEmptyValue(value))
					success = Utils::getDoubleValue(value, doubleValue);
			}

			if (success)
				values.push_back(doubleValue);
			else
			{
				success = false;
				break;
			}
		}
	}

	if (success)
		setColumnAsScale(values);

	return success;
}

bool Column::changeColumnType(Column::ColumnType newColumnType)
{
	if (newColumnType == _columnType)
		return true;

	bool success = true;
	if (newColumnType == ColumnTypeScale)
		success = _changeColumnToScale();
	else
		success = _changeColumnToNominalOrOrdinal(newColumnType);

	return success;
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

map<int, string> Column::setColumnAsNominalText(const vector<string> &values)
{
	return setColumnAsNominalText(values, map<string, string>());
}

map<int, string> Column::setColumnAsNominalText(const vector<string> &values, const map<string, string>&labels)
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

string Column::_getLabelFromKey(int key) const
{
	if (key == INT_MIN)
		return Utils::emptyValue;

	if (_labels.size() > 0)
		return _labels.getLabelObjectFromKey(key).text();

	stringstream ss;
	ss << key;

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

void Column::setValue(int row, int value)
{
	BlockMap::iterator itr = _blocks.upper_bound(row);

	if (itr == _blocks.end())
	{
		//qDebug() << "Column::setValue(), bad rowIndex";
		return;
	}

	int blockId = itr->first;
	DataBlock *block = itr->second.get();

	int blockIndex = row - blockId + DataBlock::capacity();
	block->Data[blockIndex].i = value;
}

void Column::setValue(int row, double value)
{
	BlockMap::iterator itr = _blocks.upper_bound(row);

	if (itr == _blocks.end())
	{
		//qDebug() << "Column::setValue(), bad rowIndex";
		return;
	}

	int blockId = itr->first;
	DataBlock *block = itr->second.get();

	int blockIndex = row - blockId + DataBlock::capacity();
	block->Data[blockIndex].d = value;
}

bool Column::isValueEqual(int row, double value)
{
	if (row >= _rowCount)
		return false;

	if (_columnType == Column::ColumnTypeScale)
	{
		double d = AsDoubles[row];
		if (isEmptyValue(value))
			return isEmptyValue(d);
		else
			return d == value;
	}

	return false;
}

bool Column::isValueEqual(int row, int value)
{
	if (row >= _rowCount)
		return false;

	if (_columnType == Column::ColumnTypeScale)
		return AsDoubles[row] == value;

	int intValue = AsInts[row];
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
		Label label = _labels.getLabelObjectFromKey(intValue);
		if (label.hasIntValue())
			return label.value() == value;
	}
	return false;
}

bool Column::isValueEqual(int row, const string &value)
{
	if (row >= _rowCount)
		return false;

	bool result = false;
	switch (_columnType)
	{
		case Column::ColumnTypeScale:
		{
			double v = AsDoubles[row];
			stringstream s;
			s << v;
			string str = s.str();
			result = (str == value);
			break;
		}

		case Column::ColumnTypeNominal:
		case Column::ColumnTypeOrdinal:
		{
			int v = AsInts[row];
			stringstream s;
			s << v;
			string str = s.str();
			result = (str == value);
			break;
		}
		default:
		{
			int key = AsInts[row];
			if (key == INT_MIN)
			{
				result = isEmptyValue(value);
			}
			else
			{
				result = (value.substr(0, 128) == _labels.getValueFromKey(key));
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
			int key = AsInts[row];
			if (key == INT_MIN)
				result = Utils::emptyValue;
			else
				result = _labels.getValueFromKey(key);
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
			int key = AsInts[row];
			result = _getLabelFromKey(key);
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

string Column::getColumnTypeAsString(ColumnType type)
{
	string result;
	switch (type)
	{
	case ColumnTypeScale:
		result = "Scale";
		break;
	case ColumnTypeOrdinal:
		result = "Ordinal";
		break;
	case ColumnTypeNominalText:
	case ColumnTypeNominal:
	default:
		result = "Nominal";
	}

	return result;
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

Column *Column::IntsStruct::getParent() const
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

Column *Column::DoublesStruct::getParent() const
{
	// Apparently there was a good reason to get the parent in this weird way.
	// If I try to add a private _parent (initialized with the column),
	// the engine crashes when it tries to get the dataset in the shared memory...
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
