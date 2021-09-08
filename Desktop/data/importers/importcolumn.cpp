#include "importcolumn.h"
#include <cmath>
#include "utils.h"
#include "log.h"

ImportColumn::ImportColumn(ImportDataSet* importDataSet, std::string name)
	: _importDataSet(importDataSet), _name(name)
{
}

ImportColumn::~ImportColumn()
{
}


std::string ImportColumn::name() const
{
	return _name;
}


bool ImportColumn::convertVecToInt(const std::vector<std::string> &values, std::vector<int> &intValues, std::set<int> &uniqueValues, std::map<int, std::string> &emptyValuesMap)
{
	emptyValuesMap.clear();
	uniqueValues.clear();
	intValues.clear();
	intValues.reserve(values.size());

	int row = 0;

	for (const std::string &value : values)
	{
		int intValue = std::numeric_limits<int>::min();

		if (Utils::convertValueToIntForImport(value, intValue))
		{
			if (intValue != std::numeric_limits<int>::min())	uniqueValues.insert(intValue);
			else if (!value.empty())	emptyValuesMap.insert(make_pair(row, value));

			intValues.push_back(intValue);
		}
		else
			return false;

		row++;
	}

	return true;
}

bool ImportColumn::convertVecToDouble(const std::vector<std::string> &values, std::vector<double> &doubleValues, std::map<int, std::string> &emptyValuesMap)
{
	emptyValuesMap.clear();
	doubleValues.clear();
	doubleValues.reserve(values.size());

	int row = 0;
	for (const std::string &value : values)
	{
		double doubleValue = static_cast<double>(NAN);

		if (Utils::convertValueToDoubleForImport(value, doubleValue))
		{
			doubleValues.push_back(doubleValue);

			if (std::isnan(doubleValue) && value != Utils::emptyValue)
				emptyValuesMap.insert(std::make_pair(row, value));
		}
		else
			return false;

		row++;
	}

	return true;
}

void ImportColumn::changeName(const std::string & name)
{
	Log::log() << "Changing name of column from '" << _name << "' to '" << name << "'\n." << std::endl;

	_name = name;
}
