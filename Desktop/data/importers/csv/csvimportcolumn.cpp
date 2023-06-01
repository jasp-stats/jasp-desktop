#include "csvimportcolumn.h"
#include "timers.h"

CSVImportColumn::CSVImportColumn(ImportDataSet* importDataSet, std::string name) : ImportColumn(importDataSet, name)
{
}

CSVImportColumn::CSVImportColumn(ImportDataSet *importDataSet, std::string name, long reserve) : ImportColumn(importDataSet, name)
{
	_data.reserve(reserve);
}

CSVImportColumn::~CSVImportColumn()
{
	JASPTIMER_SCOPE(CSVImportColumn::~CSVImportColumn());
	_data.clear();
}

size_t CSVImportColumn::size() const
{
	return _data.size();
}

void CSVImportColumn::addValue(const std::string &value)
{
	_data.push_back(value);
}

const std::vector<std::string> &CSVImportColumn::getValues() const
{
	return _data;
}
