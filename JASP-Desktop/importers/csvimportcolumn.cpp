#include "csvimportcolumn.h"

using namespace std;

CSVImportColumn::CSVImportColumn(ImportDataSet* importDataSet, string name) : ImportColumn(importDataSet, name)
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

bool CSVImportColumn::isValueEqual(Column &col, size_t row) const
{
	if (row >= _data.size())
		return false;

	const string &value = _data[row];

	return isStringValueEqual(value, col, row);
}
