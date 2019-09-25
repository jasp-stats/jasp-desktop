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


string ImportColumn::name() const
{
	return _name;
}


bool ImportColumn::convertVecToInt(const vector<string> &values, vector<int> &intValues, set<int> &uniqueValues, map<int, string> &emptyValuesMap)
{
	emptyValuesMap.clear();
	uniqueValues.clear();
	intValues.clear();
	intValues.reserve(values.size());

	int row = 0;

	for (const string &value : values)
	{
		int intValue = INT_MIN;

		if (Utils::convertValueToIntForImport(value, intValue))
		{
			if (intValue != INT_MIN)	uniqueValues.insert(intValue);
			else if (!value.empty())	emptyValuesMap.insert(make_pair(row, value));

			intValues.push_back(intValue);
		}
		else
			return false;

		row++;
	}

	return true;
}

bool ImportColumn::convertVecToDouble(const vector<string> &values, vector<double> &doubleValues, map<int, string> &emptyValuesMap)
{
	emptyValuesMap.clear();
	doubleValues.clear();
	doubleValues.reserve(values.size());

	int row = 0;
	for (const string &value : values)
	{
		double doubleValue = static_cast<double>(NAN);

		if (Utils::convertValueToDoubleForImport(value, doubleValue))
		{
			doubleValues.push_back(doubleValue);

			if (std::isnan(doubleValue) && value != Utils::emptyValue)
				emptyValuesMap.insert(make_pair(row, value));
		}
		else
			return false;

		row++;
	}

	return true;
}
