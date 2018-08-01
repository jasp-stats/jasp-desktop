#include "importcolumn.h"
#include <cmath>
#include "utils.h"

using namespace std;

ImportColumn::ImportColumn(ImportDataSet* importDataSet, string name)
	: _importDataSet(importDataSet), _name(name)
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
		if (!Utils::getIntValue(strValue, intValue))
			success = false;
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
		if (!Utils::getDoubleValue(v, doubleValue))
			success = false;
	}
	else
	{
		doubleValue = NAN;
	}

	return success;
}

bool ImportColumn::convertToInt(const vector<string> &values, vector<int> &intValues, set<int> &uniqueValues, map<int, string> &emptyValuesMap)
{
	bool success = true;
	int row = 0;
	for (vector<string>::const_iterator it = values.begin(); it != values.end(); ++it, ++row)
	{
		const string &value = *it;
		int intValue = INT_MIN;
		success = convertValueToInt(value, intValue);
		if (success)
		{
			if (intValue != INT_MIN)
				uniqueValues.insert(intValue);
			else if (!value.empty())
				emptyValuesMap.insert(make_pair(row, value));
			intValues.push_back(intValue);
		}
		else
			break;
	}

	return success;
}

bool ImportColumn::convertToDouble(const vector<string> &values, vector<double> &doubleValues, map<int, string> &emptyValuesMap)
{
	bool success = true;
	int row = 0;
	for (vector<string>::const_iterator it = values.begin(); it != values.end(); ++it, ++row)
	{
		const string &value = *it;
		double doubleValue = NAN;
		success = convertValueToDouble(value, doubleValue);

		if (success)
		{
			doubleValues.push_back(doubleValue);
			if (std::isnan(doubleValue) && value != Utils::emptyValue)
				emptyValuesMap.insert(make_pair(row, value));
		}
		else
			break;
	}

	return success;
}
