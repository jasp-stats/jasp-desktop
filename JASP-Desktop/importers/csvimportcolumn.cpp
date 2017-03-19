#include "csvimportcolumn.h"
#include <boost/lexical_cast.hpp>

using namespace std;

CSVImportColumn::CSVImportColumn(string name) : ImportColumn(name)
{
}

CSVImportColumn::~CSVImportColumn()
{
}

size_t CSVImportColumn::size() const
{
	return _data.size();
}

void CSVImportColumn::addValue(const string &value)
{
	_data.push_back(value);
}

const vector<string> &CSVImportColumn::getValues() const
{
	return _data;
}

string CSVImportColumn::_deEuropeanise(const string &value) const
{
	int dots = 0;
	int commas = 0;

	for (size_t i = 0; i < value.length(); i++)
	{
		if (value[i] == '.')
			dots++;
		else if (value[i] == ',')
			commas++;
	}

	if (commas > 0)
	{
		string uneurope = value;

		if (dots > 0)
		{
			size_t i = 0;
			size_t j = 0;

			for (;i < value.size(); i++)
			{
				if (value[i] == '.')
					continue;
				uneurope[j] = value[i];

				j++;
			}

			uneurope.resize(j);
		}

		for (size_t i = 0; i < uneurope.length(); i++)
		{
			if (uneurope[i] == ',')
			{
				uneurope[i] = '.';
				break;
			}
		}

		return uneurope;
	}

	return value;
}

bool CSVImportColumn::_convertValueToInt(const string &strValue, int &intValue) const
{
	bool success = true;
	if (!Column::isEmptyValue(strValue))
	{
		try
		{
			intValue = boost::lexical_cast<int>(strValue);
		}
		catch (...)
		{
			success = false;
		}
	}
	else
	{
		intValue = INT_MIN;
	}

	return success;
}

bool CSVImportColumn::_convertValueToDouble(const string &strValue, double &doubleValue) const
{
	bool success = true;
	string v = _deEuropeanise(strValue);

	if (!Column::isEmptyValue(v))
	{
		try
		{
			doubleValue = boost::lexical_cast<double>(v);
		}
		catch (...)
		{
			success = false;
		}
	}
	else
	{
		doubleValue = NAN;
	}

	return success;
}


bool CSVImportColumn::convertToInt(vector<int> &intValues, set<int> &uniqueValues) const
{
	bool success = true;
	for (vector<string>::const_iterator it = _data.begin(); it != _data.end(); ++it)
	{
		const string &value = *it;
		int intValue = INT_MIN;
		success = _convertValueToInt(value, intValue);
		if (success)
		{
			if (intValue != INT_MIN)
				uniqueValues.insert(intValue);
			intValues.push_back(intValue);
		}
		else
			break;
	}

	return success;
}

bool CSVImportColumn::convertToDouble(vector<double> &doubleValues) const
{
	bool success = true;
	for (vector<string>::const_iterator it = _data.begin(); it != _data.end(); ++it)
	{
		const string &value = *it;
		double doubleValue = NAN;
		success = _convertValueToDouble(value, doubleValue);

		if (success)
			doubleValues.push_back(doubleValue);
		else
			break;
	}

	return success;
}

bool CSVImportColumn::isValueEqual(Column &col, size_t row) const
{
	if (row >= _data.size())
		return false;

	bool result = false;
	const string &value = _data[row];

	if (col.columnType() == Column::ColumnTypeOrdinal || col.columnType() == Column::ColumnTypeNominal)
	{
		int intValue;
		if (_convertValueToInt(value, intValue))
			result = col.isValueEqual(row, intValue);
	}
	else if (col.columnType() == Column::ColumnTypeScale)
	{
		double doubleValue;
		if (_convertValueToDouble(value, doubleValue))
			result = col.isValueEqual(row, doubleValue);
	}
	else
		result = col.isValueEqual(row, value);

	return result;
}
