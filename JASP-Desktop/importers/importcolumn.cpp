#include "importcolumn.h"
#include <boost/lexical_cast.hpp>

using namespace std;

ImportColumn::ImportColumn(string name)
	: _name(name)
{
}

ImportColumn::~ImportColumn()
{
}

bool ImportColumn::isStringValueEqual(const string &value, Column &col, size_t row)
{
	bool result = false;
	if (col.columnType() == Column::ColumnTypeOrdinal || col.columnType() == Column::ColumnTypeNominal)
	{
		int intValue;
		if (convertValueToInt(value, intValue))
			result = col.isValueEqual(row, intValue);
	}
	else if (col.columnType() == Column::ColumnTypeScale)
	{
		double doubleValue;
		if (convertValueToDouble(value, doubleValue))
			result = col.isValueEqual(row, doubleValue);
	}
	else
		result = col.isValueEqual(row, value);

	return result;
}

string ImportColumn::getName() const
{
	return _name;
}

string ImportColumn::_deEuropeanise(const string &value)
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

bool ImportColumn::convertValueToInt(const string &strValue, int &intValue)
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

bool ImportColumn::convertValueToDouble(const string &strValue, double &doubleValue)
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

bool ImportColumn::convertToInt(const vector<string> &values, vector<int> &intValues, set<int> &uniqueValues)
{
	bool success = true;
	for (vector<string>::const_iterator it = values.begin(); it != values.end(); ++it)
	{
		const string &value = *it;
		int intValue = INT_MIN;
		success = convertValueToInt(value, intValue);
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

bool ImportColumn::convertToDouble(const vector<string> &values, vector<double> &doubleValues)
{
	bool success = true;
	for (vector<string>::const_iterator it = values.begin(); it != values.end(); ++it)
	{
		const string &value = *it;
		double doubleValue = NAN;
		success = convertValueToDouble(value, doubleValue);

		if (success)
			doubleValues.push_back(doubleValue);
		else
			break;
	}

	return success;
}



