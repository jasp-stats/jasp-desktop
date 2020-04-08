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

#include "column.h"
#include "utils.h"


#include <sstream>
#include <string>

#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <cmath>
#include "log.h"

using namespace boost::interprocess;
using namespace boost;
using namespace std;

int Column::count = 0;



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

const Labels & Column::labels() const
{
	return _labels;
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
				if (!Utils::isEmptyValue(orgValue))
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
		else if (intValue != INT_MIN && Utils::isEmptyValue(intValue))
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
		setColumnType(columnType::nominalText);
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
				if (!Utils::isEmptyValue(orgValue))
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
		else if (!std::isnan(doubleValue) && Utils::isEmptyValue(doubleValue))
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

//This function is pretty hard to read...
bool Column::_resetEmptyValuesForNominalText(std::map<int, string> &emptyValuesMap, bool tryToConvert)
{
	bool				hasChanged		= false;
	int					row				= 0;
	bool				hasEmptyValues	= !emptyValuesMap.empty();
	Ints::iterator		ints			= AsInts.begin();
	Ints::iterator		end				= AsInts.end();
	vector<string>		values;
	vector<int>			intValues;
	vector<double>		doubleValues;
	set<int>			uniqueIntValues;
	map<int, string>	intLabels;
	bool				canBeConvertedToIntegers	= tryToConvert,
						canBeConvertedToDoubles		= tryToConvert;

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

				if (!Utils::isEmptyValue(orgValue))
					hasChanged = true;

				if (canBeConvertedToIntegers || canBeConvertedToDoubles)
				{
					if (Utils::isEmptyValue(orgValue))
					{
						if (canBeConvertedToIntegers)	intValues.push_back(INT_MIN);
						else							doubleValues.push_back(NAN);
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
			else //if we couldnt find the "row" in the emptyValuesMap?
			{
				values.push_back(Utils::emptyValue);

				if (canBeConvertedToIntegers)		intValues.push_back(INT_MIN);
				else if (canBeConvertedToDoubles)	doubleValues.push_back(NAN);
			}
		}
		else if (key == INT_MIN)
		{
			values.push_back(Utils::emptyValue);

			if (canBeConvertedToIntegers)		intValues.push_back(INT_MIN);
			else if (canBeConvertedToDoubles)	doubleValues.push_back(NAN);
		}
		else
		{
			string orgValue = _labels.getValueFromKey(key);
			values.push_back(orgValue);

			if (Utils::isEmptyValue(orgValue))
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
					convertToDouble = true;

				if (convertToDouble)
				{
					double doubleValue;
					if (Utils::getDoubleValue(orgValue, doubleValue))	doubleValues.push_back(doubleValue);
					else												canBeConvertedToDoubles = false;
				}
			}
		}
		row++;
	}
	// End of int for loop

	//Ok, so now we have checked the "original values" for being convertible to doubles or ints, pretty awesome.
	//But there are scenarios where the user added some nice labels to their doubles (SPSS can have labels on doubles) or ints and right now these would just be thrown away.
	//Lets check if the labels are maybe not convertible to whatever datatype all orginal values *can* be.

	if(canBeConvertedToIntegers)
		for(const Label & label : _labels)
		{
			int ignoreMe;
			if(!Utils::getIntValue(label.text(), ignoreMe))
			{
				canBeConvertedToIntegers = false;
				break;
			}
		}

	if(canBeConvertedToDoubles)
		for(const Label & label : _labels)
		{
			double ignoreMe;
			if(!Utils::getDoubleValue(label.text(), ignoreMe))
			{
				canBeConvertedToDoubles = false;
				break;
			}
		}

	//Now we haven't even checked if the labels are maybe different from the values stored in orginalString... So we might still be throwing info away

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
	switch(_columnType)
	{
	case columnType::ordinal:
	case columnType::nominal:	return _resetEmptyValuesForNominal(emptyValuesMap);
	case columnType::scale:		return _resetEmptyValuesForScale(emptyValuesMap);
	default:					return _resetEmptyValuesForNominalText(emptyValuesMap);
	}
}

void Column::setSharedMemory(managed_shared_memory *mem)
{
	_mem = mem;
	_labels.setSharedMemory(mem);
}

columnType Column::getColumnType() const
{
	return _columnType;
}

bool Column::_changeColumnToNominalOrOrdinal(enum columnType newColumnType)
{
	if (_columnType == columnType::nominal || _columnType == columnType::ordinal)
	{
		_columnType = newColumnType;
		return true;
	}

	if (_columnType == columnType::nominalText)
	{
		std::vector<int>			values;
		std::set<int>				uniqueIntValues;
		std::map<int, std::string>	intLabels;

		for (int key : AsInts)
		{
			int intValue = INT_MIN;

			if (key != INT_MIN)
			{
				std::string value = _labels.getValueFromKey(key);
				if (!Utils::isEmptyValue(value) && !Utils::getIntValue(value, intValue))
					break;
			}

			values.push_back(intValue);

			if (intValue != INT_MIN)
			{
				if (uniqueIntValues.find(intValue) != uniqueIntValues.end())
				{
					std::string label = _getLabelFromKey(key);
					uniqueIntValues.insert(intValue);
					intLabels.insert(make_pair(intValue, label));
				}
			}
		}

		if (values.size() == rowCount())
		{
			_labels.clear();
			setColumnAsNominalOrOrdinal(values, intLabels, newColumnType == columnType::ordinal);
			return true;
		}

		// nominalText to nominal: we could not make the values as integers, but
		// the column can still stay a NominalText, so it is not a failure.
		return newColumnType == columnType::nominal;
	}
	else if (_columnType == columnType::scale)
	{
		std::vector<int>	values;

		for (double doubleValue : AsDoubles)
		{
			int intValue = INT_MIN;

			if (!Utils::isEmptyValue(doubleValue) && !Utils::getIntValue(doubleValue, intValue))
				break;

			values.push_back(intValue);
		}

		if (values.size() == rowCount())
		{
			setColumnAsNominalOrOrdinal(values, newColumnType == columnType::ordinal);
			return true;
		}
		else if (newColumnType == columnType::nominal)
		{
			std::vector<string> values;

			for (double doubleValue : AsDoubles)
				if (std::isnan(doubleValue))	values.push_back(Utils::emptyValue);
				else							values.push_back(Utils::doubleToString(doubleValue));

			setColumnAsNominalText(values);
			return true;
		}
	}

	return false;
}

bool Column::_changeColumnToScale()
{
	std::vector<double> values;

	switch(_columnType)
	{
	case columnType::nominal:
	case columnType::ordinal:
		for (int intValue : AsInts)
		{
			double doubleValue = NAN;

			if (intValue != INT_MIN && !Utils::isEmptyValue(intValue))
				doubleValue = double(intValue);

			values.push_back(doubleValue);
		}
		break;

	case columnType::nominalText:

		for (int key : AsInts)
		{
			double	doubleValue = NAN;
			bool	converted	= false;

			if (key != INT_MIN)
			{
				string value = _labels.getValueFromKey(key);
				if (!Utils::isEmptyValue(value))
					converted = Utils::getDoubleValue(value, doubleValue);
			}
			else
				converted = true; //Because if key == INT_MIN then it is missing value

			if (converted)	values.push_back(doubleValue);
			else			return false;
		}
		break;
	}

	setColumnAsScale(values);
	return true;
}

bool Column::changeColumnType(enum columnType newColumnType)
{
	if (newColumnType == _columnType)		return true;
	if (newColumnType == columnType::scale)	return _changeColumnToScale();
											return _changeColumnToNominalOrOrdinal(newColumnType);
}

bool Column::overwriteDataWithScale(std::vector<double> scalarData)
{
	labels().clear();

	size_t setVals = scalarData.size();

	if(scalarData.size() != rowCount())
		scalarData.resize(rowCount());

	for(size_t setThis = setVals; setThis<scalarData.size(); setThis++)
		scalarData[setThis] = static_cast<double>(std::nanf(""));

	return setColumnAsScale(scalarData);
}

bool Column::overwriteDataWithOrdinal(std::vector<int> ordinalData, std::map<int, std::string> levels)
{
	labels().clear();

	size_t setVals = ordinalData.size();

	if(ordinalData.size() != rowCount())
		ordinalData.resize(rowCount());

	for(size_t setThis = setVals; setThis<ordinalData.size(); setThis++)
		ordinalData[setThis] = INT_MIN;

	return setColumnAsNominalOrOrdinal(ordinalData, levels, true);
}

bool Column::overwriteDataWithOrdinal(std::vector<int> ordinalData)
{
	labels().clear();

	size_t setVals = ordinalData.size();

	if(ordinalData.size() != rowCount())
		ordinalData.resize(rowCount());

	for(size_t setThis = setVals; setThis<ordinalData.size(); setThis++)
		ordinalData[setThis] = INT_MIN;

	return setColumnAsNominalOrOrdinal(ordinalData, true);
}

bool Column::overwriteDataWithNominal(std::vector<int> nominalData, std::map<int, std::string> levels)
{
	labels().clear();

	size_t setVals = nominalData.size();

	if(nominalData.size() != rowCount())
		nominalData.resize(rowCount());

	for(size_t setThis = setVals; setThis<nominalData.size(); setThis++)
		nominalData[setThis] = INT_MIN;

	return setColumnAsNominalOrOrdinal(nominalData, levels, false);
}

bool Column::overwriteDataWithNominal(std::vector<int> nominalData)
{
	labels().clear();

	size_t setVals = nominalData.size();

	if(nominalData.size() != rowCount())
		nominalData.resize(rowCount());

	for(size_t setThis = setVals; setThis<nominalData.size(); setThis++)
		nominalData[setThis] = INT_MIN;

	return setColumnAsNominalOrOrdinal(nominalData, false);
}

bool Column::overwriteDataWithNominal(std::vector<std::string> nominalData)
{
	labels().clear();

	if(nominalData.size() != rowCount())
		nominalData.resize(rowCount());

	bool changedSomething;
	setColumnAsNominalText(nominalData, &changedSomething);

	return changedSomething;
}

void Column::setDefaultValues(enum columnType columnType)
{
	if(columnType == columnType::unknown)
		columnType = _columnType;

	switch(columnType)
	{
	case columnType::scale:			overwriteDataWithScale(std::vector<double>(rowCount(), static_cast<double>(std::nanf(""))));	break;
	case columnType::ordinal:		overwriteDataWithOrdinal(std::vector<int>(rowCount(), INT_MIN));								break;
	case columnType::nominal:		overwriteDataWithNominal(std::vector<int>(rowCount(), INT_MIN));								break;
	case columnType::nominalText:	overwriteDataWithNominal(std::vector<std::string>(rowCount()));									break;
	case columnType::unknown:		throw std::runtime_error("Trying to set default values of a column with unknown column type...");
	}
	_labels.clear();
}

bool Column::setColumnAsNominalOrOrdinal(const std::vector<int> &values, bool is_ordinal)
{
	std::set<int> uniqueValues(values.begin(), values.end());
	uniqueValues.erase(INT_MIN);

	bool labelChanged	= _labels.syncInts(uniqueValues);
	bool dataChanged	= _setColumnAsNominalOrOrdinal(values, is_ordinal);

	return labelChanged || dataChanged;
}

bool Column::setColumnAsNominalOrOrdinal(const vector<int> &values, map<int, string> uniqueValues, bool is_ordinal)
{
	std::set<int> uniqueValuesData(values.begin(), values.end());
	uniqueValuesData.erase(INT_MIN);

	for(int uniqVal : uniqueValuesData)
		if(uniqueValues.count(uniqVal) == 0)
		{
			Log::log() << "Setting column '" << name() << "' data to " << (is_ordinal ? "ordinal" : "nominal") << " but it is missing a label for value (" << uniqVal << "), adding the value as label." << std::endl;
			uniqueValues[uniqVal] = std::to_string(uniqVal);
		}

	bool	labelChanged	= _labels.syncInts(uniqueValues),
			dataChanged		= _setColumnAsNominalOrOrdinal(values, is_ordinal);

	return labelChanged || dataChanged;
}


bool Column::_setColumnAsNominalOrOrdinal(const vector<int> &values, bool is_ordinal)
{
	Ints::iterator	intInputItr			= AsInts.begin();
	size_t			nb_values			= 0;
	bool			changedSomething	= false;

	for(int value : values)
	{
		if(intInputItr == AsInts.end())
			throw std::runtime_error("Column::_setColumnAsNominalOrOrdinal ran out of Ints in assigning..");

		if(*intInputItr != value)
			changedSomething = true;

		*intInputItr = value;
		intInputItr++;
		nb_values++;
	}

	while (nb_values < _rowCount)
	{
		if(*intInputItr != INT_MIN)
			changedSomething = true;

		*intInputItr = INT_MIN;
		intInputItr++;
		nb_values++;
	}

	setColumnType(is_ordinal ? columnType::ordinal : columnType::nominal);

	return changedSomething;

}

bool Column::setColumnAsScale(const std::vector<double> &values)
{
	bool changedSomething = false;
	_labels.clear();
	Doubles::iterator doubleInputItr = AsDoubles.begin();

	for(double value : values)
	{
		if(doubleInputItr == AsDoubles.end())
			throw std::runtime_error("Column::setColumnAsScale ran out of Doubles in assigning..");

		if(*doubleInputItr != value && (isnan(*doubleInputItr) != isnan(value))) //clang warns us this is unsafe but what does IT know?! If it changes it changes! Maybe clang was right after all, (nan != nan) == true...
			changedSomething = true;

		*doubleInputItr = value;
		doubleInputItr++;
	}

	setColumnType(columnType::scale);

	return changedSomething;
}

std::map<int, std::string> Column::setColumnAsNominalText(const std::vector<std::string> &values, bool * changedSomething)
{
	return setColumnAsNominalText(values, std::map<std::string, std::string>(), changedSomething);
}

std::map<int, std::string> Column::setColumnAsNominalText(const std::vector<std::string> &values, const std::map<std::string, std::string>&labels, bool * changedSomething)
{
	if(changedSomething != nullptr)
		*changedSomething = false;

	std::map<int, std::string>	emptyValuesMap;
	std::set<std::string>		cases(values.begin(), values.end());
	std::vector<std::string>	sortedCases(cases.begin(), cases.end());

	std::sort(sortedCases.begin(), sortedCases.end());

	sortedCases.erase(std::remove_if(sortedCases.begin(),	sortedCases.end(), [](std::string x){	return Utils::isEmptyValue(x);}), sortedCases.end());

	std::map<std::string, int> map = _labels.syncStrings(sortedCases, labels, changedSomething);

	auto	intInputItr = AsInts.begin();
	int		nb_values	= 0;

	for(const std::string &value : values)
	{
		if(intInputItr == AsInts.end())
			throw std::runtime_error("Column::setColumnAsNominalText ran out of Ints in assigning..");

		if (Utils::isEmptyValue(value))
		{
			if(changedSomething != nullptr && *intInputItr != INT_MIN)
				*changedSomething = true;

			*intInputItr = INT_MIN;
			if (!value.empty())
				emptyValuesMap.insert(make_pair(nb_values, value));
		}
		else
		{
			if (map.find(value) == map.end())
				throw std::runtime_error("Error when reading column " + name() + ": cannot convert it to Nominal Text");
			
			if(changedSomething != nullptr && *intInputItr != map[value])
				*changedSomething = true;

			*intInputItr = map[value];
		}

		intInputItr++;
		nb_values++;
	}

	while (nb_values < _rowCount)
	{
		if(changedSomething != nullptr && *intInputItr != INT_MIN)
			*changedSomething = true;

		*intInputItr = INT_MIN;
		intInputItr++;
		nb_values++;
	}

	setColumnType(columnType::nominalText);

	return emptyValuesMap;
}

string Column::_getLabelFromKey(int key) const
{
	if (key == INT_MIN)
		return Utils::emptyValue;

	if (_labels.size() > 0)
		try
		{
			return _labels.getLabelObjectFromKey(key).text();
		}
		catch (const labelNotFound & e)
		{
			Log::log() << "Label not found, msg: " << e.what() << "\n";
			return Utils::emptyValue;
		}

	stringstream ss;
	ss << key;

	return ss.str();
}

string Column::name() const
{
	return std::string(_name.begin(), _name.end());
}

int Column::id() const
{
	return _id;
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
		//Log::log()  << "Column::setValue(), bad rowIndex" << std::endl;
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
		//Log::log()  << "Column::setValue(), bad rowIndex" << std::endl;
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

	if (_columnType == columnType::scale)
	{
		double d = AsDoubles[row];

		if (Utils::isEmptyValue(value))
			return Utils::isEmptyValue(d);
		else
			return d == value;
	}

	return false;
}

bool Column::isValueEqual(int row, int value)
{
	if (row >= _rowCount)
		return false;

	if (_columnType == columnType::scale)
		return AsDoubles[row] == value;

	int intValue = AsInts[row];
	if (_columnType == columnType::nominal || _columnType == columnType::ordinal)
	{
		bool result = (intValue == value);
		if (!result)
			Log::log() << "Value not equal: " << intValue << " " << value << std::endl;
		return result;
	}

	if (intValue == INT_MIN)
		return value == INT_MIN;

	if (_labels.size() > 0)
	{
		try
		{
			Label label = _labels.getLabelObjectFromKey(intValue);
			if (label.hasIntValue())
				return label.value() == value;
		}
		catch (const labelNotFound & e)
		{
			Log::log() << "Label not found, msg: " << e.what() << "\n";
			return false;
		}
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
		case columnType::scale:		return std::to_string(AsDoubles[row]) == value;
		case columnType::nominal:
		case columnType::ordinal:	return std::to_string(AsInts[row]) == value;
		default:
		{
			int key = AsInts[row];
			return key == INT_MIN ? Utils::isEmptyValue(value) : (value.substr(0, 128) == _labels.getValueFromKey(key));
		}
	}

	return false;
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
	else if (Utils::isEmptyValue(v))
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
		if (_columnType == columnType::scale)
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
		if (_columnType == columnType::scale)
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

		int toInsert = min(rowsLeft, DataBlock::capacity());
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
				Log::log() << "Try to erase more blocks than existing!!" << std::endl;
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

void Column::setColumnType(enum columnType columnType)
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

Column::Ints::IntsStruct::IntsStruct()
{
}

Column *Column::IntsStruct::getParent() const
{
	Column *column		= (Column*) NULL;
	char* intsAddress	= (char*)&column->AsInts;
	char* baseAddress	= (char*)column;
	char* thisAddress	= (char*)this;

	return (Column*)(thisAddress - intsAddress + baseAddress);
}

int& Column::IntsStruct::operator [](int rowIndex)
{
	Column* parent = getParent();

	BlockMap::iterator itr = parent->_blocks.upper_bound(rowIndex);

	if (itr == parent->_blocks.end())
	{
		Log::log() << "Column::Ints[], bad rowIndex: " << rowIndex << std::endl;
		Log::log() << "Nb of blocks: " << parent->_blocks.size() << std::endl;
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


Column *Column::DoublesStruct::getParent() const
{
	// This code seems quite weird... but this is a technique to get the address of the parent object from
	// a member. We could have used offsetof function though.
	// We cannot give in the constructor of DoublesStruct or IntsStruct the pointer to the parent object (the column):
	// we are here in the shared memory, this means this object cannot contain pointer pointing to the process space.
	// Even if the Column object is also in the shared memory, it is mapped in the process space so that the Column pointer
	// still points to the process space.
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
		//Log::log()  << "Column::Ints[], bad rowIndex" << std::endl;
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

bool Column::allLabelsPassFilter() const
{
	for(const Label & label : _labels)
		if(!label.filterAllows())
			return false;
	return true;
}

bool Column::hasFilter() const
{
	if(_columnType == columnType::scale)	return false;
	else									return !allLabelsPassFilter();
}

void Column::resetFilter()
{
	for(size_t i=0; i< labels().size(); i++)
		labels()[i].setFilterAllows(true);
}

bool Column::isColumnDifferentFromStringValues(std::vector<std::string> strVals)
{
	if(strVals.size() != rowCount()) return true;

	for(int row = 0; row < strVals.size(); row++)
		switch(getColumnType())
		{
		case columnType::ordinal:
		case columnType::nominal:
		{
			int intValue;
			if(!Utils::convertValueToIntForImport(strVals[row], intValue) || !isValueEqual(row, intValue))
				return true;
			break;
		}

		case columnType::scale:
		{
			double doubleValue;
			if(!Utils::convertValueToDoubleForImport(	strVals[row], doubleValue) || !isValueEqual(row, doubleValue))
				return true;
			break;
		}

		case columnType::nominalText:
		{
			if(!isValueEqual(row, strVals[row]))
				return true;
			break;
		}

		default:
			return true;
		}

	return false;
}


